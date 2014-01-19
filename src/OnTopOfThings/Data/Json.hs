{-
Copyright (C) 2014  Ellis Whitehead

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
-}

{-# LANGUAGE OverloadedStrings #-}

module OnTopOfThings.Data.Json
where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, concat)
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, joinPath)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import DatabaseTables
import Utils
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.Types

instance ToJSON ItemForJson where
  toJSON (ItemForJson item properties) = object l where
    l = catMaybes
      [ get "uuid" itemUuid
      , getDate "created" itemCreated
      , get "creator" itemCreator
      , get "type" itemType
      , get "status" itemStatus
      , getMaybe "parent" itemParent
      , getMaybe "name" itemName
      , getMaybe "title" itemTitle
      , getMaybe "content" itemContent
      , getMaybe "stage" itemStage
      , getMaybe "closed" itemClosed
      , getMaybe "start" itemStart
      , getMaybe "end" itemEnd
      , getMaybe "due" itemDue
      , getMaybe "defer" itemDefer
      , M.lookup "tag" properties >>= \tags -> Just ("tag" .= Array (V.fromList (map (String . T.pack) tags)))
      ]
    get :: T.Text -> (Item -> String) -> Maybe Pair
    get name fn = Just (name .= (T.pack $ fn item))

    getDate :: T.Text -> (Item -> UTCTime) -> Maybe Pair
    getDate name fn = Just (name .= (T.pack $ formatISO8601 (fn item)))

    getMaybe :: T.Text -> (Item -> Maybe String) -> Maybe Pair
    getMaybe name fn = fmap (\x -> name .= String (T.pack x)) (fn item)

    getMaybeDate :: T.Text -> (Item -> Maybe UTCTime) -> Maybe Pair
    getMaybeDate name fn = fmap (\x -> name .= (T.pack $ formatISO8601 x)) (fn item)

instance FromJSON ItemForJson where
  parseJSON (Object m) = do
    item <-
      Item <$>
        m .: "uuid" <*>
        m .: "created" <*>
        m .: "creator" <*>
        m .: "type" <*>
        m .: "status" <*>
        m .:? "parent" <*>
        m .:? "name" <*>
        m .:? "title" <*>
        m .:? "content" <*>
        m .:? "stage" <*>
        m .:? "closed" <*>
        m .:? "start" <*>
        m .:? "end" <*>
        m .:? "due" <*>
        m .:? "review" <*>
        return Nothing
    properties <- makeProperties <$> m .:? "tag"
    return (ItemForJson item properties)
    where
      makeProperties :: Maybe [String] -> PropertyMap
      makeProperties tags_ = M.fromList $ catMaybes
        [ tags_ >>= \tags -> Just ("tag", tags) ]

instance ToJSON PatchHunk where
  toJSON (PatchHunk uuids diffs) = object [ "uuids" .= (map T.pack uuids), "diffs" .= diffs ]

instance FromJSON PatchHunk where
  parseJSON (Object m) = PatchHunk <$> m .: "uuids" <*> m .: "diffs"

instance ToJSON Diff where
  toJSON diff = String $ T.concat $ map T.pack l where
    l = case diff of
      DiffNull name -> ["=", name]
      DiffEqual name value -> ["=", name, " ", value]
      DiffAdd name value -> ["+", name, " ", value]
      DiffUnset name -> ["x", name]
      DiffRemove name value -> ["-", name, " ", value]
--  toJSON diff = Array $ V.fromList $ map (String . T.pack) l where
--    l = case diff of
--      DiffNull name -> ["=", name]
--      DiffEqual name value -> ["=", name, value]
--      DiffAdd name value -> ["+", name, value]
--      DiffUnset name -> ["x", name]
--      DiffRemove name value -> ["-", name, value]

instance FromJSON Diff where
  parseJSON (String t) = case T.take 1 t of
    "=" -> return $ DiffEqual (T.unpack name) (T.unpack value) where
      (name, value') = T.breakOn " " (T.tail t)
      value = T.drop 1 value'
    "+" -> return $ DiffAdd (T.unpack name) (T.unpack value) where
      (name, value') = T.breakOn " " (T.tail t)
      value = T.drop 1 value'
    _ -> fail ("Unrecognized operation in diff: "++(T.unpack t))

instance ToJSON File where
  toJSON (CopyFile time_ user_ comment_ items) = object l where
    l = catMaybes
      [ Just $ "type" .= String "copy"
      , Just $ "version" .= Number 1
      , fmap (\x -> "time" .= String ((T.pack . formatISO8601) x)) time_
      , fmap (\x -> "user" .= String (T.pack x)) user_
      , fmap (\x -> "comment" .= String (T.pack x)) comment_
      , Just $ "items" .= items
      ]
  toJSON (PatchFile1 time user comment_ hunks) = object l where
    l = catMaybes
      [ Just $ "type" .= String "patch1"
      , Just $ "version" .= Number 1
      , Just $ "time" .= String ((T.pack . formatISO8601) time)
      , Just $ "user" .= String (T.pack user)
      , fmap (\x -> "comment" .= String (T.pack x)) comment_
      , Just $ "hunks" .= hunks
      ]

instance FromJSON File where
  parseJSON (Object m) = case HM.lookup "type" m of
    Just "copy" ->
      CopyFile <$>
        m .:? "time" <*>
        m .:? "user" <*>
        m .:? "comment" <*>
        m .: "items"
    Just "patch1" ->
      PatchFile1 <$>
        m .: "time" <*>
        m .: "user" <*>
        m .:? "comment" <*>
        m .: "hunks"
    Just x -> fail ("unknown file type: "++(show x))
  parseJSON x = fail ("Unknown file object: "++(show x))
