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
, databaseProcessCommandTable
) where

import           Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource ( ResourceT)

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

databaseProcessCommandTable :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseProcessCommandTable = do
  processCommands

databaseProcessCommand :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseProcessCommand = do
  processCommands

recordToCommand :: C.CommandRecord -> Command
recordToCommand (C.CommandRecord format time user cmd args) =
  Command format (formatISO8601Millis time) (T.unpack user) (T.unpack cmd) args'
  where
    args' = BL.unpack $ encode args

processCommand command = do
  case commandCmd command of
    "add" -> do processAddCommand args
    _ -> return ()
  where
    Just args = decode (BL.pack $ commandArgs command)

--processCommands :: (PersistQuery m, PersistStore m) => m ()
processCommands = do
  l <- selectList ([] :: [Filter Command]) []
  mapM_ (\entity -> processCommand (entityVal entity)) l
