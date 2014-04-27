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
{-# LANGUAGE QuasiQuotes #-}

module OnTopOfThings.Actions.View where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.ISO8601
import Database.Persist -- (PersistValue, entityVal, insert, toPersistValue, selectList)
--import Database.Persist.Sqlite
import Database.Persist.Sqlite (SqlPersistT, runSqlite, rawSql)
import Debug.Trace
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories, takeDirectory)
import System.IO
import Text.RawString.QQ
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.Yaml as Yaml

import Args
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Parsers.ItemFormatParser
import OnTopOfThings.Parsers.NumberList
import OnTopOfThings.Parsers.ViewParser
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env
import OnTopOfThings.Actions.Utils (itemToAbsPathChain, lookupItem, uuidToItem)
import OnTopOfThings.Commands.Show
--import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase


mode_view = Mode
  { modeGroupModes = mempty
  , modeNames = ["view"]
  , modeValue = options_empty "view"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "View by query."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg (updN "query") "QUERY"))
  , modeGroupFlags = toGroup
    --[ flagReq ["folder"] (updN "folder") "FOLDER" "The folder under which to search"
    [ flagReq ["sort"] (updN "sort") "FIELD" "Field to sort by, fields may be comma separated"
    , flagNone ["help"] updHelp "display this help and exit"
    ]
  }

instance Action ActionView where
  --runAction env action | trace ("runAction") False = undefined
  runAction env action = do
    result <- view env action
    case result of
      Left msgs -> return (env, ActionResult [] False [] msgs)
      Right env1 -> return (env1, mempty)
  --actionFromOptions env opts | trace ("actionFromOptions "++(show opts)) False = undefined
  actionFromOptions env opts = do
    let queries = fromMaybe [] $ M.lookup "query" (optionsParamsN opts)
    let sorts = fromMaybe [] $ M.lookup "sort" (optionsParamsN opts)
    return (Right (ActionView queries sorts))
  actionToRecordArgs action = Nothing

data QueryData = QueryData
  { queryWhere :: Maybe String
  , queryTables :: Set.Set String
  , queryValues :: [PersistValue]
  }

data ViewItem = ViewItem
  { viewItemItem :: Item
  , viewItemProperties :: [(String, String)]
  , viewItemFolder :: FilePath
  }

data ViewData = ViewData
  { viewDataItems :: [ViewItem]
  , viewDataSortFns :: [(ViewItem -> ViewItem -> Ordering)]
  , viewDataHeaderFn :: (Maybe ViewItem -> ViewItem -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String))
  , viewDataItemFn :: (ViewItem -> SqlPersistT (NoLoggingT (ResourceT IO)) (String))
  }

view :: Env -> ActionView -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Env)
view env0 (ActionView queries sorts) = do
  -- Remove all previous indexes
  updateWhere [ItemIndex !=. Nothing] [ItemIndex =. Nothing]
  -- Handle query
  let vd0 = (ViewData [] [] showHeader showItem)
  viewsub env0 vd0 queries sorts
  where
    showHeader :: Maybe ViewItem -> ViewItem -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String)
    showHeader prevMaybe vi = do
      case prevMaybe of
        Nothing -> return $ Just (viewItemFolder vi)
        Just prev -> do
          if (viewItemFolder prev) == (viewItemFolder vi)
            then return Nothing
            else return $ Just $ "\n" ++ (viewItemFolder vi)
    showItem :: ViewItem -> SqlPersistT (NoLoggingT (ResourceT IO)) (String)
    showItem vi = do
      s <- formatItem format item
      return $ prefix ++ s
      where
        item = viewItemItem vi
        format = [r|${index "" "" "" ")  "} ${X} ${times "" " " "" " --"}${name "" " (" "" ")"}${title "" " "}${estimate "" " (" "" ")"}${tags "" " (" "," ")"}|]
        index :: Maybe Int
        index = case lookup "index" (viewItemProperties vi) of
          Nothing -> Nothing
          Just s -> readMaybe s :: Maybe Int
        index_s :: Maybe String
        index_s = fmap (\i -> "(" ++ (show i) ++ ")  ") index
        prefix = fromMaybe "" index_s

viewsub :: Env -> ViewData -> [String] -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Env)
viewsub env0 vd [] _ = do
  viewPrint vd
  return (Right env0)
viewsub env0 vd (queryString:rest) sorts = do
  let query_ = parseView queryString
  case query_ of
    Left msgs -> return (Left msgs)
    Right (ViewElement_Value "print" _) -> do
      viewPrint vd
      viewsub env0 (vd { viewDataItems = [] }) rest sorts
    Right elem -> do
      liftIO $ putStrLn $ show elem
      case constructViewQuery elem 1 of
        Left msg -> return (Left [msg])
        Right (qd, _) -> do
          liftIO $ putStrLn wheres
          liftIO $ putStrLn $ show $ queryTables qd
          liftIO $ putStrLn stmt
          liftIO $ putStrLn $ show $ queryValues qd
          --tasks' <- rawSql (T.pack stmt) [] -- [toPersistValue $ formatTime' fromTime, toPersistValue $ head l]
          items' <- rawSql (T.pack stmt) (queryValues qd)
          let items = map entityVal items'
          --let x = itemTitle $ head items
          vis <- mapM itemToViewItem items
          let vd' = vd { viewDataItems = (viewDataItems vd) ++ vis }
          --liftIO $ mapM_ (putStrLn . show . itemTitle) tasks
          --liftIO $ putStrLn $ show $ length tasks
          --return (Right env0)
          viewsub env0 vd' rest sorts
          where
            wheres = fromMaybe "" (queryWhere qd)
            tables = filter (/= "item") $ Set.toList $ queryTables qd
            froms = intercalate ", " $ "item" : (map (\s -> "property " ++ s) tables)
            whereUuid = case tables of
                              [] -> ""
                              tables -> "(" ++ s ++ ") AND " where
                                s = intercalate " AND " $ map (\table -> "item.uuid = " ++ table ++ ".uuid") tables
            whereExpr = case (whereUuid, wheres) of
                              ("", "") -> ""
                              _ -> " WHERE " ++ whereUuid ++ "(" ++ wheres ++ ")"
            stmt0 = "SELECT ?? FROM " ++ froms ++ whereExpr
            stmt = case sorts of
                          [] -> stmt0
                          _ -> stmt0 ++ " ORDER BY " ++ intercalate " " sorts

itemToViewItem :: Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ViewItem
itemToViewItem item = do
  path <- getAbsPath item
  let folder = takeDirectory path
  return $ ViewItem item [] folder

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

viewPrint :: ViewData -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
viewPrint vd = do
  let vis = viewDataItems vd
  -- TODO: sort items
  -- TODO: set item indexes
  -- Remove all previous indexes
  indexNext <- getNextIndex
  let viToIndex_l = zip vis [indexNext..]
  -- TODO: fold over items, printings headers where appropriate
  foldM_ printHeaderAndItem Nothing viToIndex_l
  --ss <- mapM (viewDataItemFn vd) vis
  --liftIO $ mapM_ putStrLn ss
  return ()
  where
    printHeaderAndItem prevMaybe (vi, index) = do
      let item = viewItemItem vi
      updateIndex item index
      let item' = item { itemIndex = Just index }
      let vi' = vi { viewItemItem = item' }
      headerMaybe <- (viewDataHeaderFn vd) prevMaybe vi
      s <- (viewDataItemFn vd) vi'
      case headerMaybe of
        Just header -> liftIO $ putStrLn header
        Nothing -> return ()
      liftIO $ putStrLn s
      return (Just vi')

newtype QueryDataAnd = QueryDataAnd QueryData
newtype QueryDataOr = QueryDataOr QueryData

extractQueryDataAnd (QueryDataAnd qd) = qd

instance Monoid QueryDataAnd where
  mempty = QueryDataAnd (QueryData Nothing mempty [])
  mappend (QueryDataAnd (QueryData Nothing _ _)) b = b
  mappend a (QueryDataAnd (QueryData Nothing _ _)) = a
  mappend (QueryDataAnd (QueryData (Just where1) tables1 values1)) (QueryDataAnd (QueryData (Just where2) tables2 values2)) =
    QueryDataAnd (QueryData (Just $ where1 ++ " AND " ++ where2) (tables1 `mappend` tables2) (values1 ++ values2))

constructViewQuery :: ViewElement -> Int -> Either String (QueryData, Int)
constructViewQuery (ViewElement_And elems) propertyIndex = case foldl step (Right ([], propertyIndex)) elems of
  Left msg -> Left msg
  Right (queries_r, propertyIndex') -> Right (extractQueryDataAnd (mconcat queries), propertyIndex') where
    queries = reverse queries_r
  where
    step :: Either String ([QueryDataAnd], Int) -> ViewElement -> Either String ([QueryDataAnd], Int)
    step (Left msg) _ = Left msg
    step (Right (r, propertyIndex)) elem = case constructViewQuery elem propertyIndex of
      Left msg -> Left msg
      Right (qd, propertyIndex') -> Right (r', propertyIndex') where
        r' = (QueryDataAnd qd) : r
constructViewQuery (ViewElement_Value field values) propertyIndex
  | Set.member field (Set.fromList ["stage", "status"]) = Right $ constructViewItemQuery field values propertyIndex
  | Set.member field (Set.fromList ["tag"]) = Right $ constructViewPropertyQuery field values propertyIndex
constructViewQuery (ViewElement_BinOp field op value) propertyIndex
  | Set.member field (Set.fromList ["estimate"]) = Right $ constructItemBinOpIntQuery "item" field op (read value :: Int) propertyIndex
  -- | field == "folder" = do
  | otherwise = Right $ constructItemBinOpStringQuery "item" field op value propertyIndex

constructViewItemQuery :: String -> [String] -> Int -> (QueryData, Int)
constructViewItemQuery field values propertyIndex = (qd, propertyIndex) where
  qd = constructViewQueryValue "item" field values

constructViewQueryValue :: String -> String -> [String] -> QueryData
constructViewQueryValue table property values = QueryData (Just wheres) tables values' where
  wheres = table ++ "." ++ property ++ " " ++ rhs
  (rhs, values') = case values of
    [] -> ("IS NULL", [])
    x:[] -> ("= ?", [toPersistValue x])
    xs -> (s, l) where
      s :: String
      s = "IN (" ++ (intercalate "," (map (\_ -> "?") xs)) ++ ")"
      l :: [PersistValue]
      l = map toPersistValue xs
  tables = Set.fromList [table]

constructItemBinOpStringQuery :: String -> String -> String -> String -> Int -> (QueryData, Int)
constructItemBinOpStringQuery table property op value propertyIndex = (QueryData (Just wheres) tables values', propertyIndex) where
  wheres = table ++ "." ++ property ++ " " ++ rhs
  rhs = constructItemBinOpQueryRhs op
  values' = [toPersistValue value]
  tables = Set.fromList [table]

constructItemBinOpIntQuery :: String -> String -> String -> Int -> Int -> (QueryData, Int)
constructItemBinOpIntQuery table property op value propertyIndex = (QueryData (Just wheres) tables values', propertyIndex) where
  wheres = table ++ "." ++ property ++ " " ++ rhs
  rhs = constructItemBinOpQueryRhs op
  values' = [toPersistValue value]
  tables = Set.fromList [table]

constructItemBinOpQueryRhs :: String -> String
constructItemBinOpQueryRhs op = rhs where
  rhs = case op of
    "=" -> "= ?"
    "<" -> "< ?"
    "<=" -> "<= ?"

constructViewPropertyQuery :: String -> [String] -> Int -> (QueryData, Int)
constructViewPropertyQuery field values propertyIndex = (qd, propertyIndex') where
  qd = QueryData (Just s) tables values'
  s = tableName ++ ".name = '" ++ field ++ "' AND " ++ tableName ++ ".value = ?"
  tableName = "property" ++ show propertyIndex
  tables = Set.fromList [tableName]
  --values = map toPersistValue values
  values' = [toPersistValue $ head values]
  propertyIndex' = propertyIndex + 1


formatItem :: String -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
formatItem format item = case parseItemFormat format of
  Left msgs -> return (intercalate ";" msgs)
  Right elems -> do
    l <- mapM (\elem -> formatItemElem elem item) elems
    return (concat l)

formatItemElem :: ItemFormatElement -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) String
formatItemElem (ItemFormatElement_String s) _ = return s
formatItemElem (ItemFormatElement_Call name missing prefix infix_ suffix) item = do
  ss <- case name of
    "index" -> return $ maybeToList $ fmap show (itemIndex item)
    "X" ->
      return $ case (itemType item, itemStatus item) of
        ("list", "open") -> []
        ("list", "closed") -> ["[x]"]
        ("list", "deleted") -> ["XXX"]
        ("task", "open") -> ["[ ]"]
        (_, "open") -> [" - "]
        (_, "closed") -> ["[x]"]
        (_, "deleted") -> ["XXX"]
        _ -> []
    "name" -> return (maybeToList $ itemName item)
    "times" ->
      return $ case (itemStart item, itemEnd item, itemDue item) of
        (Just start, Just end, Just due) -> [start ++ " - " ++ end ++ ", due " ++ due]
        (Just start, Just end, Nothing) -> [start ++ " - " ++ end]
        (Just start, Nothing, Just due) -> [start ++ ", due " ++ due]
        (Just start, Nothing, Nothing) -> [start]
        (Nothing, Just end, Just due) -> ["end " ++ end ++ ", due " ++ due]
        (Nothing, Just end, Nothing) -> [end]
        (Nothing, Nothing, Just due) -> ["due " ++ due]
        _ -> []
    "title" -> return (maybeToList $ itemTitle item)
    "estimate" -> do
      return (maybeToList (fmap (\n -> show n ++ "min") $ itemEstimate item))
    "tags" -> do
      let l1 = maybeToList $ (itemStage item >>= \stage -> Just ('?':stage))
      tags' <- selectList [PropertyUuid ==. itemUuid item, PropertyName ==. "tag"] []
      let l2 = map (\x -> ('+' : (propertyValue $ entityVal x))) tags'
      return (l1 ++ l2)
    _ -> return ["unknown format spec: "++name]
  let s = case ss of
            [] -> missing
            _ -> prefix ++ (intercalate infix_ ss) ++ suffix
  return s

updateIndex :: Item -> Int -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
updateIndex item index = do
  updateWhere [ItemUuid ==. (itemUuid item)] [ItemIndex =. Just index]
