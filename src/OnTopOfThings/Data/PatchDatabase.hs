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

module OnTopOfThings.Data.PatchDatabase where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.ISO8601
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories)
import System.IO
import Database.Persist --(insert)
import Database.Persist.Class
import Database.Persist.Sql
import Database.Persist.Sqlite
import Debug.Trace
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
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
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.Types

patch :: Patch -> Bool -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
patch patch doSetIndex = do
  result_ <- mapM (patchHunk patch doSetIndex) (patchHunks patch)
  return $ fmap (const ()) (concatEithersN result_)

patchFile1 :: File -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
patchFile1 (PatchFile1 time user _ hunks) = do
  let patch' = Patch time user hunks
  patch patch' False

patchHunk :: Patch -> Bool -> PatchHunk -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
patchHunk _ _ hunk | trace ("patchHunk: "++(show hunk)) False = undefined
patchHunk header doSetIndex (PatchHunk uuids diffs) = do
  index_ <- getNextIndex
  result_ <- mapM patchone uuids
  return $ concatEithersN result_ >>= const (Right ())
  where
    --getNextIndex' :: SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe Int)
    getNextIndex' = case doSetIndex of
      False -> return Nothing
      True -> getNextIndex
    patchone :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
    patchone uuid = do
      entity_ <- getBy $ ItemUniqUuid uuid
      case entity_ of
        -- Create a new item
        Nothing -> do
          let item_ = createItem header uuid diffs
          case item_ of
            Left msgs -> return (Left msgs)
            Right (ItemForJson item properties) -> do
              insert item
              mapM_
                (\(name, values) -> do
                  mapM_ (\value -> do
                    let property = Property "item" (itemUuid item) name value
                    insert property
                    ) values
                ) (M.toList properties)
              return (Right ())
        Just entity -> do
          let item_ = updateItem header diffs (entityVal entity)
          case item_ of
            Left msgs -> return (Left msgs)
            Right item -> do
              replace (entityKey entity) item
              let maps = diffsToMaps diffs
              -- Remove property values
              mapM_
                (\(name, values) -> do
                  deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name, PropertyValue <-. values]
                ) (M.toList (diffMapsRemove maps))
              -- Add property values
              mapM_
                (\(name, values) -> do
                  mapM_ (\value -> insert (Property "item" uuid name value)) values
                ) (M.toList (diffMapsAdd maps))
              return (Right ())

getNextIndex = do
  --rawQuery "SELECT MAX(`index`) FROM item" [] $$ CL.mapM_ (liftIO . print)
  x <- rawSql "SELECT MAX(`index`) FROM item" [] -- :: SqlPersistT (NoLoggingT (ResourceT IO)) [PersistValue]
  liftIO $ print (x :: [Single Int])
  --x <- rawQuery "SELECT MAX(`index`) FROM item" []
  --let x = rawQuery "SELECT MAX(`index`) FROM item" [] $$ CL.mapM (\y -> y)
  --x $$ CL.mapM_ (liftIO . print)
  --case x of
    --[PersistInt64 n] -> return $ Just (fromIntegral n :: Int)
    --_ -> return Nothing
  case x of
    [single] -> do
      liftIO $ print (fromIntegral (unSingle single) :: Int)
      return $ Just 1
    _ -> return Nothing
  return Nothing

createItem :: Patch -> String -> [Diff] -> Validation ItemForJson
createItem header uuid diffs = do
  type_ <- get "type"
  status <- get "status"
  parent <- getMaybe "parent"
  name <- getMaybe "name"
  title <- getMaybe "title"
  content <- getMaybe "content"
  stage <- getMaybe "stage"
  closed <- getMaybe "closed"
  start <- getMaybe "start"
  end <- getMaybe "end"
  due <- getMaybe "due"
  defer <- getMaybe "defer"
  estimate <- getMaybeInt "estimate"
  let item = Item uuid created creator type_ status parent name title content stage closed start end due defer estimate Nothing
  return (ItemForJson item properties)
  where
    maps = diffsToMaps diffs
    map = diffMapsEqual maps
    properties = diffMapsAdd maps
    creator = patchUser header
    created = patchTime header
    get name = case M.lookup name map of
      Just x -> Right x
      _ -> Left ["missing value for `" ++ name ++ "`"]
    getMaybe name = case M.lookup name map of
      Just s -> Right (Just s)
      _ -> Right Nothing
    getMaybeInt name = case M.lookup name map of
      Just s -> Right (Just $ (read s :: Int))
      _ -> Right Nothing
    getMaybeDate :: String -> Validation (Maybe UTCTime)
    getMaybeDate name = case M.lookup name map of
      Just s ->
        (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
      _ -> Right Nothing

updateItem :: Patch -> [Diff] -> Item -> Validation Item
updateItem header diffs item0 = do
  type_ <- get "type" itemType
  status <- get "status" itemStatus
  parent <- getMaybe "parent" itemParent
  name <- getMaybe "name" itemName
  title <- getMaybe "title" itemTitle
  content <- getMaybe "content" itemContent
  stage <- getMaybe "stage" itemStage
  closed <- getMaybe "closed" itemClosed
  start <- getMaybe "start" itemStart
  end <- getMaybe "end" itemEnd
  due <- getMaybe "due" itemDue
  defer <- getMaybe "defer" itemDefer
  estimate <- getMaybeInt "estimate" itemEstimate
  return $ Item uuid created creator type_ status parent name title content stage closed start end due defer estimate index
  where
    maps = diffsToMaps diffs
    map = diffMapsEqual maps
    uuid = (itemUuid item0)
    creator = itemCreator item0
    created = itemCreated item0
    index = itemIndex item0
    get :: String -> (Item -> String) -> Validation String
    get name fn = case M.lookup name map of
      Just s -> Right s
      _ -> Right (fn item0)

    getMaybe :: String -> (Item -> Maybe String) -> Validation (Maybe String)
    getMaybe name fn = case M.lookup name map of
      Just s -> Right (Just s)
      _ -> Right (fn item0)

    getMaybeInt :: String -> (Item -> Maybe Int) -> Validation (Maybe Int)
    getMaybeInt name fn = case M.lookup name map of
      Just s -> Right (Just (read s :: Int))
      _ -> Right (fn item0)

    getMaybeDate :: String -> (Item -> Maybe UTCTime) -> Validation (Maybe UTCTime)
    getMaybeDate name fn = case M.lookup name map of
      Just s -> (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
      _ -> Right (fn item0)
