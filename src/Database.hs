{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Database
( Command(..)
, processCommandRecords
, xyz
, databaseBuildCommandTable
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

--processCommandRecords :: (MonadBaseControl IO m, MonadIO m) => [C.CommandRecord] -> m ()
processCommandRecords :: [C.CommandRecord] -> IO ()
processCommandRecords records = runSqlite ":memory:" $ do
  xyz records

databaseBuildCommandTable :: [C.CommandRecord] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseBuildCommandTable records = do
  runMigration migrateAll
--  processRecords records
  mapM_ processRecord records

databaseProcessCommandTable :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseProcessCommandTable = do
  processCommands

xyz :: [C.CommandRecord] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
xyz records = do
  runMigration migrateAll

  processRecords records
  l <- selectList ([] :: [Filter Command]) []
  liftIO $ print (l :: [Entity Command])

  processCommands

  l <- selectList ([] :: [Filter Property]) []
  liftIO $ mapM_ print (l :: [Entity Property])

  where
    processRecords records = case records of
      [] -> return ()
      record : rest -> do
        processRecord record
        processRecords rest

--    processRecord (C.CommandRecord format time user cmd args) =
--      insert $ Command format (formatISO8601Millis time) (T.unpack user) (T.unpack cmd) args' where
--      args' = BL.unpack $ encode args

processRecord :: PersistStore m => C.CommandRecord -> m ()
processRecord (C.CommandRecord format time user cmd args) = do
  insert $ Command format (formatISO8601Millis time) (T.unpack user) (T.unpack cmd) args'
  return ()
  where
    args' = BL.unpack $ encode args

--processCommands :: (PersistQuery m, PersistStore m) => m ()
processCommands = do
  l <- selectList ([] :: [Filter Command]) []
  mapM_ fn l
  where
    fn entity = do
      case commandCmd command of
        "add" -> do processAddCommand args
        _ -> return ()
      where
        command = entityVal entity
        Just args = decode (BL.pack $ commandArgs command)
