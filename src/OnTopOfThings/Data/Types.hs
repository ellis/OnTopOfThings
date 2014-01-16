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

module OnTopOfThings.Data.Types
where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.List (concat)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601)
import Data.Time.LocalTime (TimeOfDay, TimeZone)
import System.Directory (getDirectoryContents)
import System.FilePath (joinPath, splitDirectories, takeExtension)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import DatabaseTables
import Utils
--import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.Patch

type PropertyMap = M.Map String [String]

data Time = Time Day (Maybe TimeOfDay) (Maybe TimeZone)

data ItemForJson = ItemForJson Item PropertyMap
  deriving (Show)

data File
  = PatchFile1
    { patchFileTime :: UTCTime
    , patchFileUser :: String
    , patchFileComment :: Maybe String
    , patchFileHunks :: [PatchHunk]
    }
  | CopyFile
    { copyFileTime :: Maybe UTCTime
    , copyFileUser :: Maybe String
    , copyFileComment :: Maybe String
    , copyFileItems :: [ItemForJson]
    }
  | CommandFile
  deriving (Show)

instance Show Time where
  show (Time day time tz) = show (day, time, tz)
