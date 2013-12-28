{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Database
( Command(..)
, databaseInit
, databaseAddRecord
, databaseAddRecords
, databaseUpdateIndexes
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
--import Database.Persist.Sqlite
import Database.Esqueleto
import Database.Persist.TH

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import qualified Command as C
import Add (processAddCommand)
import Mod (processModCommand)
import DatabaseTables

databaseInit :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseInit =
  runMigration migrateAll

databaseAddRecord :: C.CommandRecord -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseAddRecord record = do
  let command = recordToCommand record
  insert command
  processCommand command
  return ()

databaseAddRecords :: [C.CommandRecord] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseAddRecords records = do
  mapM_ insert' records
  load --records'
  where
    insert' record = do
      let command = recordToCommand record
      insert command
      --return command
    load = do
      l <- select $ from $ \t -> do
        orderBy [asc (t ^. CommandTime)]
        return t
      mapM_ (processCommand . entityVal) l

-- Set index value on open tasks
databaseUpdateIndexes :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseUpdateIndexes = do
  -- Get a list of open tasks (type=task, status=open)
  entities <- select $ from $ \t -> do
    where_ (t ^. ItemType ==. val "task" &&. t ^. ItemStatus ==. val "open")
    orderBy [asc (t ^. ItemCtime)]
    return t
  let xs = (zip [1..] entities) :: [(Int, Entity Item)]
  mapM_ assignIndex xs
  where
    --assignIndex :: (Int, Entity Item) 
    assignIndex (index, entity) = do
      update $ \t -> do
        set t [ItemIndex =. val (Just index)]
        --where_ (t ^. ItemId ==. val (entityKey entity)) -- TODO: How to set by key?
        where_ (t ^. ItemUuid ==. val (itemUuid $ entityVal entity))

--databaseProcessCommandTable :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
--databaseProcessCommandTable = do
--  l <- selectList ([] :: [Filter Command]) []
--  mapM_ (\entity -> processCommand (entityVal entity)) l

recordToCommand :: C.CommandRecord -> Command
recordToCommand (C.CommandRecord format time user cmd args) =
  Command format time (T.unpack user) (T.unpack cmd) args'
  where
    args' = BL.unpack $ encode args

processCommand command = do
  case commandCmd command of
    "add" -> do processAddCommand time args
    "mod" -> do processModCommand time args
    _ -> return ()
  where
    time = commandTime command
    Just args = decode (BL.pack $ commandArgs command)

