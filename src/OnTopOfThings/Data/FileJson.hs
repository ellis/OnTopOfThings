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

module OnTopOfThings.Data.FileJson
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
import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.Patch

data File
  = PatchFile
    { patchFileTime :: Maybe UTCTime
    , patchFileUser :: Maybe String
    , patchFileComment :: Maybe String
    , patchFileHunks :: [PatchHunk]
    }
  | ExportFile
    { patchFileTime :: Maybe UTCTime
    , patchFileUser :: Maybe String
    , patchFileComment :: Maybe String
    , patchFileItems :: [ItemForJson]
    }
  | CommandFile
  deriving (Show)

instance ToJSON File where
  toJSON (ExportFile time_ user_ comment_ items) = object l where
    l = catMaybes
      [ Just $ "type" .= String "export"
      , fmap (\x -> "time" .= String ((T.pack . formatISO8601) x)) time_
      , fmap (\x -> "user" .= String (T.pack x)) user_
      , fmap (\x -> "comment" .= String (T.pack x)) comment_
      , Just $ "items" .= items
      ]
