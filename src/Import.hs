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
import Data.List (inits, intersperse, sortBy)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import Debug.Hood.Observe
import Debug.Trace
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

processImportCommand :: String -> IO ()
processImportCommand filename = do
  --s <- getContents
  --mapM_ print $ catMaybes $ map checkLine $ zip [1..] $ lines s
  input <- B.readFile filename
  case convert input of
    Left msgs -> mapM_ print msgs
    Right records -> do
      putStrLn "["
      mapM_ (\record -> putStrLn $ ((BL.unpack . encode) record ++ ",")) (init records)
      putStrLn $ (BL.unpack . encode) (last records)
      putStrLn "]"

--checkLine :: (Int, String) -> Maybe Int
--checkLine (i, s) = result where
--  s' = case reverse (strip s) of
--    ',':rest -> reverse rest
--    _ -> s
--  result = case ((decode (BL.pack s')) :: Maybe Value) of
--    Nothing -> Just i
--    _ -> Nothing

convert :: BL.ByteString -> Validation [CommandRecord]
convert input =
  case eitherDecode input of
    Left msg -> Left [msg]
    Right (Array l) -> records where
      (_, records') = foldl createItem' (Set.empty, []) (V.toList l)
      records'' = concatEithersN (reverse records')
      records = case records'' of
        Left msgs -> Left msgs
        Right records''' -> Right items where
          items = sortBy compareRecordTime records'''
      where
        createItem'
          :: (Set.Set T.Text, [Validation CommandRecord])
          -> Value
          -> (Set.Set T.Text, [Validation CommandRecord])
        createItem' (uuids, records) (Object m) = case createItem uuids m of
          Left msgs -> (uuids, Left msgs : records)
          Right (uuids', record) -> (uuids', (Right record) : records)
        createItem' (uuids, records) _ = (uuids, (Left ["Expected object"]) : records)
    --x -> Left ["Expected an array, got" ++ show input]

compareRecordTime :: CommandRecord -> CommandRecord -> Ordering
compareRecordTime a b = compare (commandTime a) (commandTime b)

convertObject :: Value -> Validation [CommandRecord]
convertObject (Object m) = case convertObject' m Set.empty Set.empty of
  Left msgs -> Left msgs
  Right (_, _, l) -> Right l
convertObject _ = Left ["Expected an object"]

convertObject' :: Object -> Set.Set [T.Text] -> Set.Set T.Text -> Validation (Set.Set [T.Text], Set.Set T.Text, [CommandRecord])
convertObject' m projects uuids = do
  uuid <- get "uuid" m
  entry' <- get "entry" m
  project' <- getMaybe "project" m
  description <- getMaybe "description" m
  status' <- getMaybe "status" m
  time <- (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack entry')) `maybeToValidation` ["Could not parse time"]
  let (projects', projectRecords) = createProjects project' time uuid
  return (projects', uuids, projectRecords)
  where
    createProjects :: Maybe T.Text -> UTCTime -> T.Text -> (Set.Set [T.Text], [CommandRecord])
    createProjects project' time uuid = case project' of
      Nothing -> (projects, [])
      Just label' -> (projects', records) where
        paths = filter (not . null) $ inits $ T.splitOn "." label'
        pathsNew = filter (\x -> not $ Set.member x projects) paths
        (projects', recordsR) = foldl createProject (projects, []) pathsNew
        records = reverse recordsR
        createProject :: (Set.Set [T.Text], [CommandRecord]) -> [T.Text] -> (Set.Set [T.Text], [CommandRecord])
        createProject (projects, records) path = (projects', CommandRecord 1 time "default" "add" args : records) where
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

createItem :: Set.Set T.Text -> Object -> Validation (Set.Set T.Text, CommandRecord)
--createItem uuids m | trace ("createItem " ++ show uuids) False = undefined
createItem uuids m = do
  uuid <- get "uuid" m
  entry' <- get "entry" m
  description <- get "description" m
  project <- getMaybe "project" m
  status' <- getMaybe "status" m
  time <- (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack entry')) `maybeToValidation` ["Could not parse time"]
  let cmd = if Set.member uuid uuids then "mod" else "add"
  let status = getStatus status'
  let parent = project >>= Just . (T.replace "." "/")
  let args = catMaybes [Just (T.concat ["id=", uuid]), Just "type=task", Just (T.concat ["title=", description]), parent >>= (\x -> Just (T.concat ["parent=", x])), Just (T.concat ["status=", status]), Just "stage=incubator"]
  return (Set.insert uuid uuids, CommandRecord 1 time "default" cmd args)
  where
    getStatus status' = case status' of
      Just "completed" -> "closed"
      Just "deleted" -> "deleted"
      _ -> "open"


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

