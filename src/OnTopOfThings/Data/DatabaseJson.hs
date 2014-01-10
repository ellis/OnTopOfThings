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

data ItemForJson = ItemForJson Item

instance ToJSON ItemForJson where
  toJSON (ItemForJson item) = object l where
    l = catMaybes
      [ get "uuid" itemUuid
      , get "type" itemType
      , get "creator" itemCreator
      , get "status" itemStatus
      , getMaybe "parent" itemParent
      , getMaybe "name" itemName
      , getMaybe "title" itemTitle
      , getMaybe "content" itemContent
      , getMaybe "stage" itemStage
      --, getMaybeDate "closed" itemClosed
      --, getMaybeDate "start" itemStart
      --, getMaybeDate "end" itemEnd
      --, getMaybeDate "due" itemDue
      --, getMaybeDate "review" itemReview
      ]
    get :: T.Text -> (Item -> String) -> Maybe Pair
    get name fn = Just (name .= (T.pack $ fn item))

    getMaybe :: T.Text -> (Item -> Maybe String) -> Maybe Pair
    getMaybe name fn = fmap (\x -> name .= (T.pack x)) (fn item)

    --getMaybeDate :: String -> (Item -> Maybe UTCTime) -> Validation (Maybe UTCTime)
    --getMaybeDate name fn = case M.lookup name map of
      --Just s -> (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
      --_ -> Right (fn item0)
