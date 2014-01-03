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

module OnTopOfThings.Commands.Show
( modeInfo_show
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (intercalate, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format (parseTime, formatTime)
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
import Database.Persist.Sqlite (SqlPersistT, runSqlite)
import Database.Esqueleto
import Debug.Trace
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
import OnTopOfThings.Commands.Utils
import OnTopOfThings.Parsers.NumberList

type PropertyMap = M.Map String [String]
type EntityMap = M.Map String PropertyMap

modeInfo_show :: ModeInfo
modeInfo_show = (mode_show, ModeRunIO optsRun_show)

mode_show = Mode
  { modeGroupModes = mempty
  , modeNames = ["show"]
  , modeValue = options_empty "show"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Display items"
  , modeHelpSuffix = []
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["from"] (upd1 "from") "TIME" "Starting time for the listing. (default=today)"
    , flagReq ["stage", "s"] (updN "stage") "STAGE" "Stage to restrict display to.  May contain a comma-separated list."
    , flagReq ["status"] (updN "status") "STATUS" "Status to restrict display to.  May contain a comma-separated list."
    , flagReq ["tag", "t"] (updN "tag") "TAG" "A comma-separated list of tags to restrict display to."
    , flagReq ["parent", "p"] (updN "parent") "ID" "ID of parent whose children should be displayed.  May contain a comma-separated list."
    , flagNone ["hide-tags"] (upd0 "hide-tags") "Display item tags along with item."
    , flagHelpSimple updHelp
    ]
  }

optsRun_show :: Options -> IO (Validation ())
optsRun_show opts = do
  now <- getCurrentTime
  let fromTime = (parseTime defaultTimeLocale "%Y%m%d" $ formatTime defaultTimeLocale "%Y%m%d" now) :: Maybe UTCTime
  case fromTime of
    Nothing -> return (Left ["bad time"])
    Just t -> do
      runSqlite "otot.db" $ do
        let parents0 = fromMaybe [] $ M.lookup "parent" (optionsParamsN opts)
        parents_l_ <- mapM fn parents0
        let parents_ = concatEithersN parents_l_
        case parents_ of
          Left msgs -> return (Left msgs)
          Right parents' -> do
            let opts_ = optionsReplaceParamN "parent" (concat parents') opts
            case opts_ of
              Left msgs -> return (Left msgs)
              Right opts' -> do
                showTasks opts' t
                return (Right ())
  where
    fn :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation [String])
    fn value =
      case parseNumberList value of
        Left msgs -> return (Left msgs)
        Right ids1 -> do
          uuids_ <- mapM refToUuid ids1
          --liftIO $ print uuids_
          return $ concatEithersN uuids_

expr' opts fromTime t = expr3 where
  m = optionsMap opts
  -- select tasks which are open or were just closed today
  expr0 = (t ^. ItemType ==. val "task" &&. (t ^. ItemStatus ==. val "open" ||. t ^. ItemClosed >. val (Just fromTime)))
  -- restrict status
  expr1 = case split (M.lookup "status" m) of
    [] -> expr0
    l -> expr0 &&. (in_ (t ^. ItemStatus) (valList l))
  -- restrict stage
  expr2 = case splitMaybe (M.lookup "stage" m) of
    [] -> expr1
    l -> expr1 &&. (in_ (t ^. ItemStage) (valList l))
  -- restrict parent
  expr3 = case M.lookup "parent" (optionsParamsN opts) of
    Nothing -> expr2
    Just l -> expr2 &&. (in_ (t ^. ItemParent) (valList (map Just l)))
  split :: Maybe (Maybe String) -> [String]
  split (Just (Just s)) = splitOn "," s
  split _ = []
  splitMaybe :: Maybe (Maybe String) -> [Maybe String]
  splitMaybe (Just (Just s)) = l where
    l' = splitOn "," s
    l = map Just l'
  splitMaybe _ = []

showTasks :: Options -> UTCTime -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
showTasks opts fromTime | trace ("showTasks: "++(show opts)) False = undefined
showTasks opts fromTime = do
  -- Load all tasks that meet the user's criteria
  tasks' <- case M.lookup "tag" (optionsParamsN opts) of
    Nothing -> do
      select $ from $ \t -> do
        where_ (expr' opts fromTime t)
        return t
    Just l ->
      select $ from $ \(i, p) -> do
        where_ ((expr' opts fromTime i) &&. p ^. PropertyUuid ==. i ^. ItemUuid &&. p ^. PropertyName ==. val "tag" &&. (in_ (p ^. PropertyValue) (valList l)))
        return i
  let tasks = map entityVal tasks'
  --liftIO $ print tasks
  -- Recursively load all parent items
  items <- loadRecursive tasks
  --liftIO $ mapM_ (putStrLn . itemToString) items
  -- Get the lists
  let lists = (filter (\item -> itemType item == "list") items) :: [Item]
  --liftIO $ print lists
  let children = concat $ map (filterChildren items) lists :: [Item]
  -- Remove all previous indexes
  update $ \t -> do
    set t [ItemIndex =. nothing]
    where_ (not_ $ isNothing (t ^. ItemIndex))
  let itemToIndex_l = zip children [1..]
  let uuidToIndex_m = M.fromList $ map (\(item, index) -> (itemUuid item, index)) itemToIndex_l
  -- Set new indexes
  mapM updateIndex itemToIndex_l
  --mapM setIndex children
  -- Get the items in the order they'll be printed
  let ordered = concat $ map (\parent -> parent : (filterChildren items parent)) lists
  --liftIO $ print ordered
  mapM_ (fn uuidToIndex_m) ordered
  return ()
  where
    updateIndex :: (Item, Int) -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    updateIndex (item, index) = do
      update $ \t -> do
        set t [ItemIndex =. val (Just index)]
        where_ (t ^. ItemUuid ==. val (itemUuid item))
    fn :: M.Map String Int -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    fn uuidToIndex_m item =
      case itemType item of
        "list" -> do
          liftIO $ putStrLn ""
          s <- itemToString opts item
          liftIO $ putStrLn $ s
        _ -> do
          s <- itemToString opts item
          liftIO $ putStrLn $ prefix ++ s
          where
            index :: Maybe Int
            index = M.lookup (itemUuid item) uuidToIndex_m
            index_s :: Maybe String
            index_s = fmap (\i -> "(" ++ (show i) ++ ")  ") index
            prefix = fromMaybe "" index_s

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

-- Get the children of the given 'parent' from the 'items'
filterChildren :: [Item] -> Item -> [Item]
filterChildren items parent =
  filter (\item -> itemParent item == Just (itemUuid parent)) items

--findParentLabel :: Maybe [String] -> EntityMap -> [String]
--findParentLabel uuid m = findParentLabel' uuid m []

--findParentLabel' :: Maybe [String] -> EntityMap -> [String] -> [String]
--findParentLabel' (Just [uuid]) m acc =
--  -- See whether entity exists
--  case M.lookup uuid m of
--    Nothing -> acc
--    Just m' -> findParentLabel' uuid' m acc' where
--      -- Update acc with label or title
--      acc' = case M.lookup "label" m' of
--        Nothing ->
--          case M.lookup "title" m' of
--            Nothing -> acc
--            Just title -> acc ++ title
--        Just label -> acc ++ label
--      -- Continuing by search for this items parent
--      uuid' = M.lookup "parent" m'
--findParentLabel' _ _ acc = acc

itemToString :: Options -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
itemToString opts item = do
  tags <- if Set.member "hide-tags" (optionsParams0 opts)
    then return ([] :: [String])
    else do
      tags' <- select $ from $ \t -> do
        where_ (t ^. PropertyUuid ==. val (itemUuid item) &&. t ^. PropertyName ==. val "tag")
        return (t ^. PropertyValue)
      return $ map (\(Value s) -> '+':s) tags'
  let l = getParts tags
  return $ unwords l
  where
    path = [] -- findParentLabel (itemParent item >>= \x -> Just [x]) entities
    check :: Maybe String
    check = case (itemType item, itemStatus item) of
      ("list", "open") -> Nothing
      ("list", "closed") -> Just "[x]"
      ("list", "deleted") -> Just "XXX"
      ("task", "open") -> Just "[ ]"
      (_, "open") -> Just "-"
      (_, "closed") -> Just "[x]"
      (_, "deleted") -> Just "- XXX"
      _ -> Nothing
    getParts :: [String] -> [String]
    getParts tags = catMaybes
      [ check
      , fmap (\label -> "(" ++ label ++ ")") (itemLabel item)
      , if null path then Nothing else Just $ intercalate "/" path ++ ":"
      , Just $ itemTitle item
      , if null tags then Nothing else Just $ "(" ++ (intercalate "," tags) ++ ")"
      ]

