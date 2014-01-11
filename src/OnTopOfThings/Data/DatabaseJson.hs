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

module OnTopOfThings.Data.DatabaseJson
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

data ExportJson = ExportJson
  { exportVersion :: Int
  , exportTime :: UTCTime
  , exportComment :: String
  , exportItems :: [ItemForJson]
  } deriving (Show)

data ItemForJson = ItemForJson Item
  deriving (Show)

instance ToJSON ExportJson where
  toJSON (ExportJson version time comment items) = object l where
    l =
      [ "version" .= version
      , "time" .= (T.pack $ formatISO8601 time)
      , "comment" .= (T.pack comment)
      , "items" .= items
      ]

instance ToJSON ItemForJson where
  toJSON (ItemForJson item) = object l where
    l = catMaybes
      [ get "uuid" itemUuid
      , getDate "ctime" itemCtime
      , get "creator" itemCreator
      , get "type" itemType
      , get "status" itemStatus
      , getMaybe "parent" itemParent
      , getMaybe "name" itemName
      , getMaybe "title" itemTitle
      , getMaybe "content" itemContent
      , getMaybe "stage" itemStage
      , getMaybeDate "closed" itemClosed
      , getMaybeDate "start" itemStart
      , getMaybeDate "end" itemEnd
      , getMaybeDate "due" itemDue
      , getMaybeDate "review" itemReview
      ]
    get :: T.Text -> (Item -> String) -> Maybe Pair
    get name fn = Just (name .= (T.pack $ fn item))

    getDate :: T.Text -> (Item -> UTCTime) -> Maybe Pair
    getDate name fn = Just (name .= (T.pack $ formatISO8601 (fn item)))

    getMaybe :: T.Text -> (Item -> Maybe String) -> Maybe Pair
    getMaybe name fn = fmap (\x -> name .= (T.pack x)) (fn item)

    getMaybeDate :: T.Text -> (Item -> Maybe UTCTime) -> Maybe Pair
    getMaybeDate name fn = fmap (\x -> name .= (T.pack $ formatISO8601 x)) (fn item)


