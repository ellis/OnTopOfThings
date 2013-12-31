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
import Add (processCommand_add, processCommand_mod)
import DatabaseTables
import Utils

databaseInit :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
databaseInit =
  runMigration migrateAll

databaseAddRecord :: C.CommandRecord -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
databaseAddRecord record = do
  let command = recordToCommand record
  insert command
  processCommand command

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

processCommand :: Command -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
processCommand command = do
  case commandCmd command of
    "add" -> processCommand_add time args
    "mod" -> processCommand_mod time args
    cmd -> return $ Left ["processCommand: Unknown command `"++cmd++"`"]
  where
    time = commandTime command
    Just args = decode (BL.pack $ commandArgs command)

