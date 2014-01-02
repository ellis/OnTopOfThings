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

module OnTopOfThings.Commands.List
( modeInfo_list
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (intercalate, nub, sort)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format (parseTime, formatTime)
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
import Database.Persist.Sqlite (SqlPersistT, runSqlite)
import Database.Esqueleto
import System.Console.CmdArgs.Explicit
import System.Locale (defaultTimeLocale)
import Text.Regex (mkRegex, matchRegexAll)

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Command as C

import Args
import DatabaseTables
import Utils

type PropertyMap = M.Map String [String]
type EntityMap = M.Map String PropertyMap

modeInfo_list :: ModeInfo
modeInfo_list = (mode_list, ModeRunIO optsRun_list)

mode_list = Mode
  { modeGroupModes = mempty
  , modeNames = ["list"]
  , modeValue = options_empty "list"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "List items"
  , modeHelpSuffix = []
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["from"] (upd "from") "TIME" "Starting time for the listing. (default=today)"
    , flagHelpSimple updHelp
    ]
  }

optsRun_list :: Options -> IO (Validation ())
optsRun_list opts = do
  now <- getCurrentTime
  let fromTime = (parseTime defaultTimeLocale "%Y%m%d" $ formatTime defaultTimeLocale "%Y%m%d" now) :: Maybe UTCTime
  case fromTime of
    Nothing -> return (Left ["bad time"])
    Just t -> do
      runSqlite "otot.db" $ do
        listTasks t
      return (Right ())

optsValidate :: Options -> Validation ()
optsValidate opts = Right ()

listHandler :: [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
listHandler args = do
  -- parse args, for the moment we'll ignore them and pretend "from=today"
  now <- liftIO $ getCurrentTime
  let fromTime = (parseTime defaultTimeLocale "%Y%m%d" $ formatTime defaultTimeLocale "%Y%m%d" now) :: Maybe UTCTime
  case fromTime of
    Just t -> listTasks t
    Nothing -> liftIO $ putStrLn "bad time"

listTasks :: UTCTime -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
listTasks fromTime = do
  -- Load all tasks that are open or were closed today
  tasks' <- select $ from $ \t -> do
    where_ (t ^. ItemType ==. val "task" &&. (t ^. ItemStatus ==. val "open" ||. t ^. ItemClosed >. val (Just fromTime)))
    return t
  let tasks = map entityVal tasks'
  -- Recursively load all parent items
  items <- loadRecursive tasks
  --liftIO $ mapM_ (putStrLn . itemToString) items
  -- Get the lists
  let lists = filter (\item -> itemType item == "list") items
  liftIO $ mapM_ (printListAndTasks items) lists
  return ()

loadRecursive :: [Item] -> SqlPersistT (NoLoggingT (ResourceT IO)) [Item]
loadRecursive items = loadParents items uuids items where
  uuids = Set.fromList $ map itemUuid items

loadParents :: [Item] -> Set.Set String -> [Item] -> SqlPersistT (NoLoggingT (ResourceT IO)) [Item]
loadParents [] _ itemsAll = return itemsAll
loadParents itemsCurrent uuidsOld itemsAll = do
  let parents' = (Set.fromList . catMaybes) $ map itemParent itemsCurrent
  let uuidsNext = Set.difference parents' uuidsOld
  itemsNext <- loadByUuid (Set.toList uuidsNext)
  loadParents itemsNext (Set.union uuidsOld parents') (itemsAll ++ itemsNext)

--loadByUuid :: [T.Text] -> [Item]
loadByUuid uuids = do
  item_ll <- mapM fn uuids
  return $ concat item_ll
  where
    fn uuid = do
      entities <- select $ from $ \t -> do
        where_ (t ^. ItemUuid ==. val uuid)
        return t
      return $ map entityVal entities

printListAndTasks :: [Item] -> Item -> IO ()
printListAndTasks items list = do
  putStrLn ""
  putStrLn $ itemToString list
  let items' = filter (\item -> itemParent item == Just (itemUuid list)) items
  mapM_ (putStrLn . itemToString) items'

fn1' :: [Property] -> EntityMap
fn1' properties = fn1 properties M.empty

fn1 :: [Property] -> EntityMap -> EntityMap
fn1 [] acc = acc
fn1 ((Property table uuid name value) : rest) acc = fn1 rest acc' where
  acc' = case M.lookup uuid acc of
    Nothing -> M.insert uuid M.empty acc
    Just m -> M.insert uuid m' acc where
      m' = M.insert name [value] m

findParentLabel :: Maybe [String] -> EntityMap -> [String]
findParentLabel uuid m = findParentLabel' uuid m []

findParentLabel' :: Maybe [String] -> EntityMap -> [String] -> [String]
findParentLabel' (Just [uuid]) m acc =
  -- See whether entity exists
  case M.lookup uuid m of
    Nothing -> acc
    Just m' -> findParentLabel' uuid' m acc' where
      -- Update acc with label or title
      acc' = case M.lookup "label" m' of
        Nothing ->
          case M.lookup "title" m' of
            Nothing -> acc
            Just title -> acc ++ title
        Just label -> acc ++ label
      -- Continuing by search for this items parent
      uuid' = M.lookup "parent" m'
findParentLabel' _ _ acc = acc

itemToString :: Item -> String
itemToString item = unwords l where
  path = [] -- findParentLabel (itemParent item >>= \x -> Just [x]) entities
  check :: Maybe String
  check = case (itemType item, itemStatus item) of
    ("list", "open") -> Nothing
    ("list", "closed") -> Just "[x]"
    ("list", "deleted") -> Just "XXX"
    ("task", "open") -> Just "- [ ]"
    (_, "open") -> Just "- "
    (_, "closed") -> Just "- [x]"
    (_, "deleted") -> Just "- XXX"
    _ -> Nothing
  l :: [String]
  l = catMaybes $
    [ check
    , itemIndex item >>= (\x -> Just ("(" ++ (show x) ++ ")"))
    , if null path then Nothing else Just $ intercalate "/" path ++ ":"
    , Just $ itemTitle item
    ]

itemToString' :: Item -> EntityMap -> String
itemToString' item entities = unwords l where
  path = findParentLabel (itemParent item >>= \x -> Just [x]) entities
  check :: Maybe String
  check = case (itemType item, itemStatus item) of
    ("list", "open") -> Nothing
    ("list", "closed") -> Just "[x]"
    ("list", "deleted") -> Just "XXX"
    ("task", "open") -> Just "- [ ]"
    (_, "open") -> Just "- "
    (_, "closed") -> Just "- [x]"
    (_, "deleted") -> Just "- XXX"
    _ -> Nothing
  l :: [String]
  l = catMaybes $
    [ check
    , itemIndex item >>= (\x -> Just ("(" ++ (show x) ++ ")"))
    , if null path then Nothing else Just $ intercalate "/" path ++ ":"
    , Just $ itemTitle item
    ]

--itemToString :: String -> PropertyMap -> EntityMap -> String
--itemToString uuid properties entities = unwords l where
--  path = findParentLabel (M.lookup "parent" properties) entities
--  isTask :: Bool
--  isTask = (M.lookup "type" properties) == Just ["task"]
--  check :: String
--  check = case M.lookup "status" properties of
--    Just ["open"] -> if isTask then " [ ]" else ""
--    Just ["closed"] -> " [x]"
--    Just ["deleted"] -> " XXX"
--    _ -> ""
--  l :: [String]
--  l = catMaybes $
--    [ Just $ "-" ++ check
--    , M.lookup "index" properties >>= (\x -> Just ("(" ++ unwords x ++ ")"))
--    , if null path then Nothing else Just $ intercalate "/" path ++ ":"
--    , M.lookup "title" properties >>= Just . unwords
--    ]
