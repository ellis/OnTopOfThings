{-
Copyright (C) 2013,2014  Ellis Whitehead

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

module OnTopOfThings.Commands.Import
( modeInfo_import
, optsToCommandRecord
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum)
import Data.Generics.Aliases (orElse)
import Data.List (inits, intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mempty)
import Data.Time.Clock
import Data.Time.LocalTime (utc)
import Data.Time.Format (parseTime)
import Data.Time.ISO8601 (formatISO8601, formatISO8601Millis)
import Debug.Hood.Observe
import Debug.Trace
import System.Console.CmdArgs.Explicit
import System.Directory (getDirectoryContents)
import System.FilePath (joinPath, splitDirectories, takeExtension, takeFileName)
import System.IO
import System.Locale (defaultTimeLocale)
--import qualified System.FilePath.Find as FF
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import Args
import Command (CommandRecord(..))
import DatabaseTables
import Utils
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Json
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.Time
import OnTopOfThings.Data.Types

modeInfo_import :: ModeInfo
modeInfo_import = (mode_import, ModeRunIO optsRun_import)

mode_import = Mode
  { modeGroupModes = mempty
  , modeNames = ["import"]
  , modeValue = options_empty "import"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Import task warrior JSON"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg updArgs "ID"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["output", "o"] (upd "output") "FILE" "file to use for output"
    , flagHelpSimple updHelp
    ]
  }

optsValidate :: Options -> Validation ()
optsValidate opts = do
  when (null $ optionsArgs opts) (Left ["You must supply a filename"])
  Right ()

optsRun_import :: Options -> IO (Validation ())
optsRun_import opts = do
  case optsValidate opts of
    Left msgs -> return (Left msgs)
    Right () -> do
      let filename = head (optionsArgs opts)
      --mapM_ print $ catMaybes $ map checkLine $ zip [1..] $ lines s
      input <- B.readFile filename
      let m = (optionsMap opts)
      h <- case M.lookup "output" m of
        Just (Just f) -> openFile f WriteMode
        _ -> return stdout
      case convert' input of
        Left msgs -> return (Left msgs)
        Right items' -> do
          time <- getCurrentTime
          let items = sortBy (\(ItemForJson a _) (ItemForJson b _) -> compare (itemCreated a) (itemCreated b)) items'
          let export = CopyFile (Just time) (Just "default") (Just "Task Warrior import") items
          --BS.hPutStr h $ Yaml.encode export
          BL.hPutStr h $ encode export
          hClose h
          return (Right ())

convert' :: BL.ByteString -> Validation [ItemForJson]
convert' input = l_ where
  l_ = case eitherDecode input of
    Left msg -> Left [msg]
    Right (Array l) -> items_ where
      db' = createItems' (V.toList l) (M.empty, M.empty)
      items_ = case db' of
        Left msgs -> Left msgs
        Right db -> Right l where
          l = M.elems db
          -- TODO: need to sort l
  createItems' :: [Value] -> (M.Map String ItemForJson, M.Map FilePath Item) -> Validation (M.Map String ItemForJson)
  createItems' [] (db, _) = Right db
  createItems' ((Object m):rest) both = createItems both m >>= \both -> createItems' rest both
  createItems' _ _ = Left ["Expected object"]
  --x -> Left ["Expected an array, got" ++ show input]

createItems :: (M.Map String ItemForJson, M.Map FilePath Item) -> Object -> Validation (M.Map String ItemForJson, M.Map FilePath Item)
--createItems uuids projects m | trace ("createItems " ++ show uuids) False = undefined
createItems (db, folders) m = do
  uuid <- get "uuid" m
  entry' <- get "entry" m
  description <- get "description" m
  project <- getMaybe "project" m
  status' <- getMaybe "status" m
  end' <- getMaybe "end" m
  tags <- getList "tags" m
  created <- (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" entry') `maybeToValidation` ["Could not parse entry time"]
  --closed <- (end' >>= \end -> (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack end))) `maybeToValidation` ["Could not parse end time"]
  closed <- getClosed end'
  let parentPath_ = project >>= Just . (substituteInList '.' '/')
  let (db', folders') = case parentPath_ of
                          Nothing -> (db, folders)
                          Just parentPath -> createFolder (db, folders) parentPath created
  let parentUuid_ = fmap (T.unpack . getProjectUuid . T.pack) parentPath_
  let parentUuid = fromMaybe uuidRoot parentUuid_
  let item = Item { itemUuid = uuid
                  , itemCreated = created
                  , itemCreator = "default"
                  , itemType = "task"
                  , itemStatus = getStatus status'
                  , itemParent = Just parentUuid
                  , itemName = Nothing
                  , itemTitle = Just description
                  , itemContent = Nothing
                  , itemStage = Just "new"
                  , itemClosed = closed
                  , itemStart = Nothing
                  , itemEnd = Nothing
                  , itemDue = Nothing
                  , itemDefer = Nothing
                  , itemIndex = Nothing
                  }
  let properties = if null tags then M.empty else M.fromList [("tag", tags)]
  let json = ItemForJson item properties
  let db'' = M.insert uuid json db'
  return (db'', folders')
  --let mode = if Set.member (T.pack uuid) uuids then mode_mod else mode_add
  --let status = getStatus status'
  --let parent = project >>= Just . (substituteInList '.' '/')
  --let parentUuid = fmap (T.unpack . getProjectUuid . T.pack) parent
  --let args = catMaybes [wrap "id" uuid, wrap "title" description, wrapMaybe "parent" parentUuid, wrapMaybe "status" status, wrapMaybeTime "closed" closed, wrapList "tag" tags']
  --let projects' = updateProjects (fmap T.pack parent) time
  --opts0 <- eitherStringToValidation $ process mode args
  --let record = optsToCommandRecord time "default" opts0
  --let uuids' = Set.insert (T.pack uuid) uuids
  --return (uuids', projects', record)
  where
    getStatus status' = case status' of
      Just "completed" -> "closed"
      Just "deleted" -> "deleted"
      _ -> "open"
    getClosed :: Maybe String -> Validation (Maybe String)
    getClosed closed' = case closed' of
      Just closed'' ->
        case parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" closed'' of
          Nothing -> Left ["Could not parse end time: " ++ closed'']
          Just x -> Right (Just (formatTime' $ zonedToTime x))
      Nothing -> Right Nothing
--    updateProjects :: Maybe T.Text -> UTCTime -> M.Map T.Text UTCTime
--    updateProjects parent time = case parent of
--      Nothing -> projects
--      Just p -> M.insert p t projects where
--        t = fromMaybe time (M.lookup p projects >>= \t -> Just $ min time t)

uuidRoot = "00000000-0000-0000-0000-000000000000"

createFolder :: (M.Map String ItemForJson, M.Map FilePath Item) -> FilePath -> UTCTime -> (M.Map String ItemForJson, M.Map FilePath Item)
createFolder both@(db, lists) path time =
  case M.lookup path lists of
    Just _ -> both
    Nothing -> (db', folders') where
      dirs = splitDirectories path
      path_l = map joinPath $ tail $ inits dirs
      (db', folders') = createFolders both path_l time uuidRoot

createFolders :: (M.Map String ItemForJson, M.Map FilePath Item) -> [FilePath] -> UTCTime -> String -> (M.Map String ItemForJson, M.Map FilePath Item)
createFolders both [] _ _ = both
createFolders (db, folders) (path:rest) time parentUuid = createFolders (db', folders') rest time uuid where
  uuid = T.unpack $ getProjectUuid (T.pack path)
  folder = Item
    { itemUuid = uuid
    , itemCreated = time
    , itemCreator = "default"
    , itemType = "folder"
    , itemStatus = "open"
    , itemParent = Just parentUuid
    , itemName = Just (takeFileName path)
    , itemTitle = Nothing
    , itemContent = Nothing
    , itemStage = Nothing
    , itemClosed = Nothing
    , itemStart = Nothing
    , itemEnd = Nothing
    , itemDue = Nothing
    , itemDefer = Nothing
    , itemIndex = Nothing
    }
  db' = M.insert uuid (ItemForJson folder M.empty) db
  folders' = M.insert path folder folders

--convert :: BL.ByteString -> Validation [CommandRecord]
--convert input =
--  case eitherDecode input of
--    Left msg -> Left [msg]
--    Right (Array l) -> records where
--      (_, projectMap, itemRecords') = foldl createItem' (Set.empty, M.empty, []) (V.toList l)
--      records = case concatEithersN (reverse itemRecords') of
--        Left msgs -> Left msgs
--        Right itemRecords'' -> Right l where
--          itemRecords = sortBy compareRecordTime itemRecords''
--          projectRecords = map createProject $ M.toList (trace ("projectMap: " ++ (show projectMap)) projectMap)
--          l = sortBy compareRecordTime (projectRecords ++ itemRecords)
--          --l = sortBy compareRecordTime (projectRecords)
--      where
--        createItem'
--          :: (Set.Set T.Text, M.Map T.Text UTCTime, [Validation CommandRecord])
--          -> Value
--          -> (Set.Set T.Text, M.Map T.Text UTCTime, [Validation CommandRecord])
--        createItem' (uuids, projects, records) (Object m) = case createItem uuids projects m of
--          Left msgs -> (uuids, projects, Left msgs : records)
--          Right (uuids', projects', record) -> (uuids', projects', (Right record) : records)
--        createItem' (uuids, projects, records) _ = (uuids, projects, (Left ["Expected object"]) : records)
--    --x -> Left ["Expected an array, got" ++ show input]

--compareRecordTime :: CommandRecord -> CommandRecord -> Ordering
--compareRecordTime a b = compare (Command.commandTime a) (Command.commandTime b)

--createItem :: Set.Set T.Text -> M.Map T.Text UTCTime -> Object -> Validation (Set.Set T.Text, M.Map T.Text UTCTime, CommandRecord)
----createItem uuids projects m | trace ("createItem " ++ show uuids) False = undefined
--createItem uuids projects m = do
--  uuid <- get "uuid" m
--  entry' <- get "entry" m
--  description <- get "description" m
--  project <- getMaybe "project" m
--  status' <- getMaybe "status" m
--  end' <- getMaybe "end" m
--  tags' <- getList "tags" m
--  time <- (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" entry') `maybeToValidation` ["Could not parse entry time"]
--  --closed <- (end' >>= \end -> (parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack end))) `maybeToValidation` ["Could not parse end time"]
--  closed <- getClosed end'
--  let mode = if Set.member (T.pack uuid) uuids then mode_mod else mode_add
--  let status = getStatus status'
--  let parent = project >>= Just . (substituteInList '.' '/')
--  let parentUuid = fmap (T.unpack . getProjectUuid . T.pack) parent
--  let args = catMaybes [wrap "id" uuid, wrap "title" description, wrapMaybe "parent" parentUuid, wrapMaybe "status" status, wrapMaybeTime "closed" closed, wrapList "tag" tags']
--  let projects' = updateProjects (fmap T.pack parent) time
--  opts0 <- eitherStringToValidation $ process mode args
--  let record = optsToCommandRecord time "default" opts0
--  let uuids' = Set.insert (T.pack uuid) uuids
--  return (uuids', projects', record)
--  where
--    getStatus status' = case status' of
--      Just "completed" -> Just "closed"
--      Just "deleted" -> Just "deleted"
--      _ -> Nothing
--    getClosed :: Maybe String -> Validation (Maybe UTCTime)
--    getClosed closed' = case closed' of
--      Just closed'' ->
--        case parseTime defaultTimeLocale "%Y%m%dT%H%M%SZ" closed'' of
--          Nothing -> Left ["Could not parse end time: " ++ closed'']
--          Just x -> Right (Just x)
--      Nothing -> Right Nothing
--    updateProjects :: Maybe T.Text -> UTCTime -> M.Map T.Text UTCTime
--    updateProjects parent time = case parent of
--      Nothing -> projects
--      Just p -> M.insert p t projects where
--        t = fromMaybe time (M.lookup p projects >>= \t -> Just $ min time t)
--    wrap :: String -> String -> Maybe String
--    wrap name value = Just (concat ["--", name, "=", value])
--    wrapMaybe :: String -> Maybe String -> Maybe String
--    wrapMaybe name value = value >>= (\x -> Just (concat ["--", name, "=", x]))
--    wrapMaybeTime :: String -> Maybe UTCTime -> Maybe String
--    wrapMaybeTime name value = value >>= (\x -> Just (concat ["--", name, "=", formatISO8601Millis x]))
--    wrapList :: String -> [String] -> Maybe String
--    wrapList _ [] = Nothing
--    wrapList name l = Just $ (concat ["--", name, "=", intercalate "," l])

get :: T.Text -> HM.HashMap T.Text Value -> Validation String
get name m = case HM.lookup name m of
  Nothing -> Left ["Missing `" ++ (T.unpack name) ++ "`"]
  Just (String text) -> Right (T.unpack text)
  Just _ -> Left ["Field `" ++ (T.unpack name) ++ "` is not text"]

getMaybe :: T.Text -> Object -> Validation (Maybe String)
getMaybe name m = case HM.lookup name m of
  Nothing -> Right Nothing
  Just (String text) -> Right $ Just (T.unpack text)
  Just _ -> Left ["Field `" ++ (T.unpack name) ++ "` is not text"]

getList :: T.Text -> Object -> Validation ([String])
getList name m = case HM.lookup name m of
  Nothing -> Right []
  Just (Array v) -> Right $ V.toList $ V.map (\(String text) -> T.unpack text) v
  Just _ -> Left ["Field `" ++ (T.unpack name) ++ "` is not and array"]

createProject :: (T.Text, UTCTime) -> CommandRecord
createProject (label, time) = CommandRecord 1 time' "default" "add" args where
  time' = addUTCTime (-1 :: NominalDiffTime) time
  -- Create a uuid from the label
  -- Use only alphanumeric lower-case characters,
  -- insert hypens at the right places (e.g. 0a815f87-ab07-45e7-abbd-21a71c21c176)
  -- and fill with 0s
  uuid = getProjectUuid label
  args = [T.concat ["--id=", uuid], "--type=list", T.concat ["--label=", label], T.concat ["--title=", label]]

getProjectUuid label = uuid where
  label' = T.toLower $ T.filter isAlphaNum label
  uuid0 = T.justifyLeft (8+4+4+4+12) '0' label'
  uuid = insertHyphen 8 $ insertHyphen (8+4) $ insertHyphen (8+4+4) $ insertHyphen (8+4+4+4) uuid0
  insertHyphen :: Int -> T.Text -> T.Text
  insertHyphen i s = T.concat [left, "-", right] where
    (left, right) = T.splitAt i s

optsToCommandRecord :: UTCTime -> T.Text -> Options -> CommandRecord
optsToCommandRecord time user opts = CommandRecord 1 time user (T.pack $ optionsCmd opts) opts'' where
  opts' = reform opts
  opts'' = map T.pack opts'
