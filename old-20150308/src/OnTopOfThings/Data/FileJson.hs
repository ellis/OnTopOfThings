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

-- REFACTOR: rename module/file
module OnTopOfThings.Data.FileJson
where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.List (concat)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601)
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
import OnTopOfThings.Data.Json
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.Types


loadFiles :: IO (Validation [Event])
loadFiles = do
  files <- getDirectoryContents "testdata"
  --files <- FF.find (return False) (FF.extension `FF.==?` ".json") "testdata"
  events__ <- mapM (\f -> loadFile (joinPath ["testdata", f])) files
  let events_ = concatEithersN events__
  let events = events_ >>= \l -> Right (Data.List.concat l)
  return events

loadFile :: FilePath -> IO (Validation [Event])
loadFile path = do
  case takeExtension path of
    ".yaml" -> do
      file_ <- Yaml.decodeFileEither path
      case file_ of
        Left msg -> return $ Left [show msg]
        Right file ->
          case file of
            CopyFile time_ user_ comment_ items ->
              return $ Right (map copyToEvent items)
            PatchFile1 time user comment_ hunks ->
              return $ Right [(patch1ToEvent time user comment_ hunks)]
    ".json" -> do
      content <- BL.readFile path
      let file_ = eitherDecode content
      case file_ of
        Left msg -> return $ Left [show msg]
        Right file ->
          case file of
            CopyFile time_ user_ comment_ items ->
              return $ Right (map copyToEvent items)
            PatchFile1 time user comment_ hunks ->
              return $ Right [(patch1ToEvent time user comment_ hunks)]
    "" -> return (Right [])
    _ -> do
      putStrLn ("unrecognized file extension: "++path)
      return (Right [])
  where
    copyToEvent :: ItemForJson -> Event
    copyToEvent wrapper@(ItemForJson item properties) = event where
      data_ = BL.toStrict $ encode wrapper
      event = Event (itemCreated item) (itemCreator item) Nothing "createItem" 1 data_
    patch1ToEvent :: UTCTime -> String -> Maybe String -> [PatchHunk] -> Event
    patch1ToEvent time user comment hunks = event where
      data_ = BL.toStrict $ encode hunks
      event = Event time user comment "patch1" 1 data_

saveFileAsJson :: File -> FilePath -> IO ()
saveFileAsJson file filename = do
  let filepath = joinPath ["testdata", filename ++ ".json"]
  BL.writeFile filepath $ encode file

eventToPatchFile1 :: Event -> Validation File
eventToPatchFile1 (Event time user comment _ version json) =
  case eitherDecode (BL.fromStrict json) of
    Left msg -> Left [msg]
    Right hunks -> Right (PatchFile1 time user comment hunks)

eventToItems :: Event -> Validation [ItemForJson]
eventToItems event =
  case eitherDecode (BL.fromStrict $ eventData event) of
    Left msg -> Left [msg]
    Right item -> Right [item]
