{-
Copyright (C) 2013  Ellis Whitehead

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

module Import
( processImportCommand
) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (inits, intersperse)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, joinPath)
import System.Locale (defaultTimeLocale)
--import qualified System.FilePath.Find as FF
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

import Add (createAddCommandRecord)
import Command (CommandRecord(..))
import Utils

--instance Monad Validation where
--  Left msg >>= f = Left msg
--  Right x >>= f = f x
--  return x = Right x

processImportCommand :: IO ()
processImportCommand = do
  s <- getContents
  print $ convert (BL.pack s)

convert :: BL.ByteString -> [Validation [CommandRecord]]
convert input = output where
  l = decode input :: Maybe [Value]
  output = case l of
    Just l -> map convertObject l
    Nothing -> [Left ["Expected an array"]]

convertObject :: Value -> Validation [CommandRecord]
convertObject (Object m) = convertObject' m Set.empty Set.empty
convertObject _ = Left ["Expected an object"]

convertObject' :: Object -> Set.Set [T.Text] -> Set.Set T.Text -> Validation [CommandRecord]
convertObject' m projects uuids = do
  uuid <- get "uuid" m
  entry' <- get "entry" m
  project' <- getMaybe "project" m
  description <- getMaybe "description" m
  status' <- getMaybe "status" m
  time <- (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack entry')) `maybeToValidation` ["Could not parse time"]
  let projects = createProjects project' time uuid
  return projects
  where
    createProjects :: Maybe T.Text -> UTCTime -> T.Text -> [CommandRecord]
    createProjects project' time uuid = case project' of
      Nothing -> []
      Just label' -> map createProject pathsNew where
        paths = inits $ T.splitOn "." label'
        pathsNew = filter (\x -> not $ Set.member x projects) paths
        createProject :: [T.Text] -> CommandRecord
        createProject path = CommandRecord 1 time "default" "add" args where
          parent :: [T.Text]
          parent = init path
          parentLabel :: T.Text
          parentLabel = T.intercalate "/" parent
          args0 :: [T.Text]
          args0 = ["type=list", T.concat ["title=", last path], T.concat ["label=", T.intercalate "/" path]]
          args :: [T.Text]
          args = case null parent of
            True -> args0
            False -> (T.concat ["parent=", parentLabel]):args0

get :: T.Text -> HM.HashMap T.Text Value -> Validation T.Text
get name m = case HM.lookup name m of
  Nothing -> Left ["Missing `" ++ (T.unpack name) ++ "`"]
  Just (String text) -> Right text
  Just _ -> Left ["Field `" ++ (T.unpack name) ++ "` is not text"]

getMaybe :: T.Text -> Object -> Validation (Maybe T.Text)
getMaybe name m = case HM.lookup name m of
  Nothing -> Right Nothing
  Just (String text) -> Right $ Just text
  Just _ -> Left ["Field `" ++ (T.unpack name) ++ "` is not text"]

