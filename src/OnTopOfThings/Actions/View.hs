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
import Database.Persist (PersistValue, entityVal, insert, toPersistValue)
--import Database.Persist.Sqlite
import Database.Persist.Sqlite (SqlPersistT, runSqlite, rawSql)
import Debug.Trace
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories)
import System.IO
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


instance Action ActionView where
  --runAction env action | trace ("runAction") False = undefined
  runAction env action = do
    result <- view env action
    case result of
      Left msgs -> return (env, ActionResult [] False [] msgs)
      Right env1 -> return (env1, mempty)
  --actionFromOptions env opts | trace ("actionFromOptions "++(show opts)) False = undefined
  actionFromOptions env opts = do
    let queries_ = M.lookup "query" (optionsParamsN opts)
    case queries_ of
      Nothing -> return (Left ["view: please supply a query"])
      Just queries ->
        --let viewElems = map parseView queries
        return (Right (ActionView queries))
  actionToRecordArgs action = Nothing

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
    [ flagNone ["help"] updHelp "display this help and exit"
    ]
  }

data QueryData = QueryData
  { queryWhere :: Maybe String
  , queryTables :: Set.Set String
  , queryValues :: [PersistValue]
  }

view :: Env -> ActionView -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Env)
view env0 (ActionView queries) = do
  let query_ = parseView (head queries)
  case query_ of
    Left msgs -> return (Left msgs)
    Right elem -> do
      liftIO $ putStrLn $ show elem
      let (qd, _) = constructViewQuery elem 1
      case queryWhere qd of
        Nothing -> return (Left ["no query found"])
        Just wheres -> do
          liftIO $ putStrLn wheres
          liftIO $ putStrLn $ show $ queryTables qd
          let tables = filter (/= "item") $ Set.toList $ queryTables qd
          let froms = intercalate ", " $ "item" : (map (\s -> "property " ++ s) tables)
          let whereUuid = case tables of
                            [] -> ""
                            tables -> "(" ++ s ++ ") AND " where
                              s = intercalate " AND " $ map (\table -> "item.uuid = " ++ table ++ ".uuid") tables
          liftIO $ putStrLn whereUuid
          let stmt = "SELECT ?? FROM " ++ froms ++ " WHERE " ++ whereUuid ++ "(" ++ wheres ++ ")"
          liftIO $ putStrLn stmt
          liftIO $ putStrLn $ show $ queryValues qd
          --tasks' <- rawSql (T.pack stmt) [] -- [toPersistValue $ formatTime' fromTime, toPersistValue $ head l]
          tasks' <- rawSql (T.pack stmt) (queryValues qd)
          let tasks = map entityVal tasks'
          let x = itemTitle $ head tasks
          liftIO $ mapM_ (putStrLn . show . itemTitle) tasks
          liftIO $ putStrLn $ show $ length tasks
          return (Right env0)

newtype QueryDataAnd = QueryDataAnd QueryData
newtype QueryDataOr = QueryDataOr QueryData

extractQueryDataAnd (QueryDataAnd qd) = qd

instance Monoid QueryDataAnd where
  mempty = QueryDataAnd (QueryData Nothing mempty [])
  mappend (QueryDataAnd (QueryData Nothing _ _)) b = b
  mappend a (QueryDataAnd (QueryData Nothing _ _)) = a
  mappend (QueryDataAnd (QueryData (Just where1) tables1 values1)) (QueryDataAnd (QueryData (Just where2) tables2 values2)) =
    QueryDataAnd (QueryData (Just $ where1 ++ " AND " ++ where2) (tables1 `mappend` tables2) (values1 ++ values2))

constructViewQuery :: ViewElement -> Int -> (QueryData, Int)
constructViewQuery (ViewElement_And elems) propertyIndex = (extractQueryDataAnd (mconcat queries), propertyIndex') where
  (queries_r, propertyIndex') = foldl step ([], propertyIndex) elems
  queries = reverse queries_r
  step :: ([QueryDataAnd], Int) -> ViewElement -> ([QueryDataAnd], Int)
  step (r, propertyIndex) elem = (r', propertyIndex') where
    (qd, propertyIndex') = constructViewQuery elem propertyIndex
    r' = (QueryDataAnd qd) : r
constructViewQuery (ViewElement_Value field values) propertyIndex
  | Set.member field (Set.fromList ["tag"]) = constructViewPropertyQuery field values propertyIndex
  | Set.member field (Set.fromList ["stage"]) = constructViewItemQuery field values propertyIndex
--constructViewQuery (ViewElement_Value field values) propertyIndex = (constructViewQueryValue "item" field values, propertyIndex)

constructViewItemQuery :: String -> [String] -> Int -> (QueryData, Int)
constructViewItemQuery field values propertyIndex = (qd, propertyIndex) where
  qd = constructViewQueryValue "item" field values

constructViewPropertyQuery :: String -> [String] -> Int -> (QueryData, Int)
constructViewPropertyQuery field values propertyIndex = (qd, propertyIndex') where
  qd = QueryData (Just s) tables values'
  s = tableName ++ ".name = '" ++ field ++ "' AND " ++ tableName ++ ".value = ?"
  tableName = "property" ++ show propertyIndex
  tables = Set.fromList [tableName]
  --values = map toPersistValue values
  values' = [toPersistValue $ head values]
  propertyIndex' = propertyIndex + 1

constructViewQueryValue :: String -> String -> [String] -> QueryData
constructViewQueryValue table property values = QueryData (Just wheres) tables values' where
  wheres = table ++ "." ++ property ++ " " ++ rhs where
  (rhs, values') = case values of
    [] -> ("IS NULL", [])
    x:[] -> ("= ?", [toPersistValue x])
    xs -> (s, l) where
      s :: String
      s = "IN (" ++ (intercalate "," (map (\_ -> "?") xs)) ++ ")"
      l :: [PersistValue]
      l = map toPersistValue xs
  tables = Set.fromList [table]