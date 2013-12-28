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
, databaseLookupUuid
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
  mapM_ databaseAddRecord records

databaseUpdateIndexes :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseUpdateIndexes = do
  -- TODO: There's a better way to do this, I'm sure -- something with joins or something.
  -- Get a list of pending tasks
  --entities1 <- selectList [PropertyTable ==. "item", PropertyName ==. "type", PropertyValue ==. "task"] []
  e <- select $ from $ \(a, b, c) -> do
    where_ (
      a ^. PropertyUuid ==. b ^. PropertyUuid &&.
      a ^. PropertyUuid ==. c ^. PropertyUuid &&.
      a ^. PropertyName ==. val "type" &&. a ^. PropertyValue ==. val "task" &&.
      b ^. PropertyName ==. val "status" &&. b ^. PropertyValue ==. val "open" &&.
      c ^. PropertyName ==. val "ctime")
    orderBy [asc (c ^. PropertyValue)]
    return (a ^. PropertyUuid)
  liftIO $ print e
  mapM_ assignIndex $ zip [1..] e
  return ()
  where
    assignIndex (index, (Value uuid)) = do
      delete $ from $ \t ->
        where_ (t ^. PropertyTable ==. val "item" &&. t ^. PropertyUuid ==. val uuid &&. t ^. PropertyName ==. val "index")
      insert $ Property "item" uuid "index" $ show index
      liftIO $ print $ Property "item" uuid "index" $ show index
  --mapM_ choose entities1
  --selectList [PropertyTable ==. "item", PropertyName ==. "type", PropertyValue ==. "task"] []

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
    _ -> return ()
  where
    time = commandTime command
    Just args = decode (BL.pack $ commandArgs command)

--databaseLookupUuid :: PersistQuery m => String -> m (Maybe String)
databaseLookupUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String)
databaseLookupUuid ref = do
  l1 <- select $ from $ \t -> do
    where_ (t ^. PropertyUuid ==. val ref)
    limit 1
    return (t ^. PropertyUuid)
  l2 <- select $ from $ \t -> do
    where_ (t ^. PropertyName ==. val "index" &&. t ^. PropertyValue ==. val ref)
    limit 1
    return (t ^. PropertyUuid)
  case l1 of
    [Value uuid] -> return $ Just uuid
    _ ->
      case l2 of
        [Value uuid] -> return $ Just uuid
        _ -> return Nothing

