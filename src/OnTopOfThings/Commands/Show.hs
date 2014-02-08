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

module OnTopOfThings.Commands.Show
( modeInfo_show
, optsRun_show
) where

import Control.Monad (liftM2, mplus, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (intercalate, nub, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Monoid
import Data.Time (Day, TimeZone, getCurrentTimeZone)
import Data.Time.Clock
import Data.Time.Format (parseTime, formatTime)
import Database.Persist
import Database.Persist.Sql (insert, deleteWhere, toPersistValue)
import Database.Persist.Sqlite (SqlPersistT, runSqlite, rawSql)
import Database.Persist.Types
import Debug.Trace
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.FilePath (joinPath)
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
import OnTopOfThings.Data.Time
import OnTopOfThings.Parsers.NumberList
import OnTopOfThings.Parsers.ItemFormatParser

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
  , modeArgs = ([], Just (flagArg updArgs "REPORT"))
  , modeGroupFlags = toGroup
    [ flagReq ["from"] (upd1 "from") "TIME" "Starting time for the listing. (default=today)"
    , flagReq ["search"] (upd1 "search") "TEXT" "Limit items to those whose title matches TEXT."
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
  tz <- getCurrentTimeZone
  --let fromTime = (parseTime defaultTimeLocale "%Y%m%d" $ formatTime defaultTimeLocale "%Y%m%d" now) :: Maybe UTCTime
  let fromTime = (parseTime defaultTimeLocale "%Y%m%d" $ formatTime defaultTimeLocale "%Y%m%d" now) :: Maybe UTCTime
  let fromTime2_ = parseTime' tz $ formatTime defaultTimeLocale "%Y-%m-%d" now
  case (fromTime, fromTime2_) of
    (Just t, Right fromTime2) -> do
      runSqlite "repl.db" $ do
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
                case optionsArgs opts of
                  [] -> do
                    showTasks opts' fromTime2
                    return (Right ())
                  ["calendar"] -> do
                    let opts__ = optionsReplaceParamN "stage" ["today"] opts'
                    case opts__ of
                      Left msgs -> return (Left msgs)
                      Right opts'' -> do
                        showCalendar opts'' fromTime2
                        return (Right ())
                  ["today"] -> do
                    let opts__ = optionsReplaceParamN "stage" ["today"] opts'
                    case opts__ of
                      Left msgs -> return (Left msgs)
                      Right opts'' -> do
                        showTasks opts'' fromTime2
                        return (Right ())
    _ -> return (Left ["bad time"])
  where
    fn :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation [String])
    fn value =
      case parseNumberList value of
        Left msgs -> return (Left msgs)
        Right ids1 -> do
          uuids_ <- mapM refToUuid ids1
          --liftIO $ print uuids_
          return $ concatEithersN uuids_

expr' opts fromTime = expr4 where
  m = optionsMap opts
  -- select tasks which are open or were just closed today
  expr0 = [ItemType ==. "task", FilterOr [ItemStatus ==. "open", ItemClosed >. Just (formatTime' fromTime)]]
  -- restrict status
  expr1 = case splitN "status" of
    [] -> expr0
    l -> expr0 ++ [ItemStatus <-. l]
  -- restrict stage
  expr2 = case splitNMaybe "stage" of
    [] -> expr1
    l -> expr1 ++ [ItemStage <-. l]
  -- restrict parent
  expr3 = case M.lookup "parent" (optionsParamsN opts) of
    Nothing -> expr2
    Just l -> expr2 ++ [ItemParent <-. (map Just l)]
  -- restrict by searching for text in title
  expr4 = case M.lookup "search" (optionsParams1 opts) of
    Nothing -> expr3
    Just s -> expr3 ++ [Filter ItemTitle (Left $ Just ("%"++s++"%")) (BackendSpecificFilter "like")]
  split :: Maybe (Maybe String) -> [String]
  split (Just (Just s)) = splitOn "," s
  split _ = []
  splitN :: String -> [String]
  splitN name = case M.lookup name (optionsParamsN opts) of
      Just l -> concat $ map (splitOn ",") l
      Nothing -> []
  splitNMaybe :: String -> [Maybe String]
  splitNMaybe name =
    case M.lookup name (optionsParamsN opts) of
      Just l -> map Just $ concat $ map (splitOn ",") l
      Nothing -> []

showTasks :: Options -> Time -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
showTasks opts fromTime | trace ("showTasks: "++(show opts)) False = undefined
showTasks opts fromTime = do
  -- Load all tasks that meet the user's criteria
  let filters = expr' opts fromTime
  tasks' <- case M.lookup "tag" (optionsParamsN opts) of
    Nothing -> selectList filters []
    Just l -> do -- selectList filters []
      let wheres =
                   [ "item.type = 'task'"
                   , "(item.status = 'open' OR item.closed >= ?)"
                   , "item.uuid = property.uuid"
                   , "property.name = 'tag'"
                   , "property.value = ?"
                   ]
      let stmt = "SELECT ?? FROM item, property WHERE " ++ (intercalate " AND " wheres)
      --liftIO $ print $ persistFieldDef ItemType
      liftIO $ putStrLn stmt
      rawSql (T.pack stmt) [toPersistValue $ formatTime' fromTime, toPersistValue $ head l]
      -- FIXME: implement this:
      --select $ from $ \(i, p) -> do
        --where_ ((expr' opts fromTime i) &&. p ^. PropertyUuid ==. i ^. ItemUuid &&. p ^. PropertyName ==. val "tag" &&. (in_ (p ^. PropertyValue) (valList l)))
        --return i
  let tasks = map entityVal tasks'
  --liftIO $ putStrLn "tasks:"
  --liftIO $ mapM_ print tasks
  -- Recursively load all parent items
  items <- loadRecursive tasks
  --liftIO $ putStrLn "items:"
  --liftIO $ mapM_ print items
  -- Get the lists
  let lists0 = (filter isContainerItem items) :: [Item]
  lists1 <- mapM (\item -> itemToString opts item >>= \s -> return (s, item)) lists0
  let lists2 = sortBy (\a b -> compare (fst a) (fst b)) lists1
  --liftIO $ putStrLn "lists:"
  --liftIO $ mapM_ (putStrLn . fst) lists2
  let lists = map snd lists2
  --liftIO $ putStrLn "lists:"
  --liftIO $ print lists
  let children = concat $ map (filterChildren items) lists :: [Item]
  let orphans = filter (\task -> (itemParent task) == Nothing) tasks
  -- Remove all previous indexes
  updateWhere [ItemIndex !=. Nothing] [ItemIndex =. Nothing]
  let itemToIndex_l = zip (children ++ orphans) [1..]
  let uuidToIndex_m = M.fromList $ map (\(item, index) -> (itemUuid item, index)) itemToIndex_l
  -- Set new indexes
  mapM updateIndex itemToIndex_l
  --mapM setIndex children
  -- Get the items in the order they'll be printed
  let ordered = concat $ map (\parent -> parent : (filterChildren items parent)) lists
  --liftIO $ print ordered
  mapM_ (fn uuidToIndex_m) ordered
  if not (null orphans)
    then do
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Tasks without a list:"
      mapM_ (fn uuidToIndex_m) orphans
    else return ()
  return ()
  where
    updateIndex :: (Item, Int) -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    updateIndex (item, index) = do
      updateWhere [ItemUuid ==. (itemUuid item)] [ItemIndex =. Just index]
    fn :: M.Map String Int -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    fn uuidToIndex_m item
      | isContainerItem item = do
        liftIO $ putStrLn ""
        s <- itemToString opts item
        liftIO $ putStrLn $ s
      | otherwise = do
        s <- itemToString opts item
        liftIO $ putStrLn $ prefix ++ s
        where
          index :: Maybe Int
          index = M.lookup (itemUuid item) uuidToIndex_m
          index_s :: Maybe String
          index_s = fmap (\i -> "(" ++ (show i) ++ ")  ") index
          prefix = fromMaybe "" index_s

showCalendar :: Options -> Time -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
showCalendar opts fromTime | trace ("showCalendar: "++(show opts)) False = undefined
showCalendar opts fromTime = do
  tz <- liftIO $ getCurrentTimeZone
  let fromTime_s = formatTime' fromTime
  -- Load all tasks that meet the user's criteria
  let filters = expr' opts fromTime
  entities <- do
    let wheres =
                 --[ "(item.type = 'event' OR item.type = 'task')"
                 [ "(item.start >= ? OR item.closed >= ? OR item.due >= ?)"
                 ]
    let stmt = "SELECT ?? FROM item WHERE " ++ (intercalate " AND " wheres)
    liftIO $ putStrLn stmt
    rawSql (T.pack stmt) [toPersistValue fromTime_s, toPersistValue fromTime_s, toPersistValue fromTime_s]
  let tasks' = map entityVal entities
  liftIO $ putStrLn "tasks':"
  liftIO $ mapM_ print tasks'
  let timeToItem_l_ = (concatEithersN $ map (\item -> itemToTime tz item >>= \time -> Right (time, item)) tasks') >>= \l ->
                      Right $ sortBy (\a b -> compare (fst a) (fst b)) l
  case timeToItem_l_ of
    Left msgs -> liftIO $ mapM_ putStrLn msgs
    Right timeToItem_l -> do
      let items = map snd timeToItem_l
      -- Remove all previous indexes
      updateWhere [ItemIndex !=. Nothing] [ItemIndex =. Nothing]
      let itemToIndex_l = zip items [1..]
      let uuidToIndex_m = M.fromList $ map (\(item, index) -> (itemUuid item, index)) itemToIndex_l
      -- Set new indexes
      mapM updateIndex itemToIndex_l
      recurse timeToItem_l uuidToIndex_m Nothing
      --mapM_ (fn uuidToIndex_m) items
      return ()
  where
    itemToTime :: TimeZone -> Item -> Validation Time
    itemToTime tz item = do
      s <- (itemStart item) `mplus` (itemClosed item) `mplus` (itemDue item) `maybeToValidation` ["INTERNAL: missing start, closed, or due time"]
      parseTime' tz s
    updateIndex :: (Item, Int) -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    updateIndex (item, index) = do
      updateWhere [ItemUuid ==. (itemUuid item)] [ItemIndex =. Just index]
    -- print list of items
    recurse :: [(Time, Item)] -> M.Map String Int -> Maybe Day -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    recurse [] _ _ = return ()
    recurse ((time, item):rest) uuidToIndex_m maybeDay = do
      let day = Just $ otimeDay time
      when (day /= maybeDay) $ liftIO $ do
        when (isJust maybeDay) (putStrLn "")
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %A" (otimeDay time)
        setSGR [Reset]
      fn uuidToIndex_m item
      recurse rest uuidToIndex_m day
    -- print an item
    fn :: M.Map String Int -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    fn uuidToIndex_m item
      | isContainerItem item = do
        liftIO $ putStrLn ""
        s <- itemToString opts item
        liftIO $ putStrLn $ s
      | otherwise = do
        s <- formatItem "${X}  ${H} ${t} ${T}" item
        liftIO $ putStrLn $ prefix ++ s
        where
          index :: Maybe Int
          index = M.lookup (itemUuid item) uuidToIndex_m
          index_s :: Maybe String
          index_s = fmap (\i -> "(" ++ (show i) ++ ")  ") index
          prefix = fromMaybe "" index_s

isContainerItem :: Item -> Bool
isContainerItem item = elem (itemType item) ["folder", "list"]

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
      entities <- selectList [ItemUuid ==. uuid] []
      return $ map entityVal entities

-- Get the children of the given 'parent' from the 'items'
filterChildren :: [Item] -> Item -> [Item]
filterChildren items parent =
  filter (\item -> (not. isContainerItem) item && itemParent item == Just (itemUuid parent)) items

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

-- ${title}
-- ${title, 'missing', 'prefix', 'infix', 'suffix'}
formatItem :: String -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
formatItem format item = parseFormatItem' format item

parseFormatItem' :: [Char] -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
parseFormatItem' [] _ = return ""
parseFormatItem' format item = do
  (s, format') <- parseFormatItem format item
  s' <- parseFormatItem' format' item
  return (s ++ s')

parseFormatItem :: [Char] -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) (String, [Char])
parseFormatItem [] _ = return ("", [])
parseFormatItem ('$':'$':rest) item = return ("$", rest)
-- TODO: would also like to handle format like: %{T^infix}, %{T^prefix^siffix}, %{T^prefix^infix^suffix}
parseFormatItem ('$':'{':x:'}':rest) item = do
  s <- formatElement (x:[]) ("", "", "") item
  return (s, rest)
parseFormatItem ('$':'{':x1:x2:'}':rest) item = do
  s <- formatElement (x1:x2:[]) ("", "", "") item
  return (s, rest)
parseFormatItem (x:rest) item = return (x:[], rest)

formatElement :: String -> (String, String, String) -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
formatElement x (prefix, infix_, suffix) item = do
  s_ <- case x of
    "X" ->
      return $ case (itemType item, itemStatus item) of
        ("list", "open") -> Nothing
        ("list", "closed") -> Just "[x]"
        ("list", "deleted") -> Just "XXX"
        ("task", "open") -> Just "[ ]"
        (_, "open") -> Just "-"
        (_, "closed") -> Just "[x]"
        (_, "deleted") -> Just "XXX"
        _ -> Nothing
    "H" ->
      return $ case (itemStart item, itemEnd item, itemDue item) of
        (Just start, Just end, Just due) -> Just (start ++ " - " ++ end ++ ", due " ++ due)
        (Just start, Just end, Nothing) -> Just (start ++ " - " ++ end)
        (Just start, Nothing, Just due) -> Just (start ++ ", due " ++ due)
        (Just start, Nothing, Nothing) -> Just (start)
        (Nothing, Just end, Just due) -> Just ("end " ++ end ++ ", due " ++ due)
        (Nothing, Just end, Nothing) -> Just (end)
        (Nothing, Nothing, Just due) -> Just ("due " ++ due)
        _ -> Nothing
    "t" -> return (itemTitle item)
    "T" -> do
      tags' <- selectList [PropertyUuid ==. itemUuid item, PropertyName ==. "tag"] []
      let l = map (\x -> '+':(propertyValue $ entityVal x)) tags'
      let s = if null l then "" else "(" ++ (intercalate ", " l) ++ ")"
      return (Just s)
  return $ fromMaybe "" $ flip fmap s_ (\s -> prefix ++ s ++ suffix)

itemToString :: Options -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
itemToString opts item = do
  tags <- if Set.member "hide-tags" (optionsParams0 opts)
    then return ([] :: [String])
    else do
      tags' <- selectList [PropertyUuid ==. itemUuid item, PropertyName ==. "tag"] []
      return $ map (\x -> '+':(propertyValue $ entityVal x)) tags'
  l <- getParts tags
  return $ unwords l
  where
    --path = findParentLabel (itemParent item >>= \x -> Just [x]) entities
    check :: Maybe String
    check = case (itemType item, itemStatus item) of
      ("list", "open") -> Nothing
      ("list", "closed") -> Just "[x]"
      ("list", "deleted") -> Just "XXX"
      ("task", "open") -> Just "[ ]"
      (_, "open") -> Just "-"
      (_, "closed") -> Just "[x]"
      (_, "deleted") -> Just "XXX"
      _ -> Nothing
    --getParts :: [String] -> [String]
    getParts tags
      | (itemType item) == "folder" = do
        path <- getAbsPath item
        return $ catMaybes [Just path, itemTitle item]
      | otherwise =
        return $ catMaybes
          [ check
          , fmap (\label -> "(" ++ label ++ ")") (itemName item)
          --, if null path then Nothing else Just $ intercalate "/" path ++ ":"
          , itemTitle item
          , if null tags then Nothing else Just $ "(" ++ (intercalate "," tags) ++ ")"
          ]

getAbsPath :: Item -> SqlPersistT (NoLoggingT (ResourceT IO)) FilePath
getAbsPath item = do
  parentPath <- case (itemParent item) of
    Nothing -> return []
    Just uuid -> do
      parent_ <- getBy $ ItemUniqUuid uuid
      case parent_ of
        Nothing -> return []
        Just parent -> do
          parentPath <- getAbsPath (entityVal parent)
          return [parentPath]
  return $ joinPath (parentPath ++ [fromMaybe (itemUuid item) (itemName item)])
