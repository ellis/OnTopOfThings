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
) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import qualified Command as C

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Command
  format Int
  time String
  user String
  cmd String
  args String
  deriving Show
Property
  table String
  uuid String
  name String
  value String
  deriving Show
|]

processCommandRecords :: [C.CommandRecord] -> IO ()
processCommandRecords records = runSqlite ":memory:" $ do
  runMigration migrateAll

  processRecords records
  l <- selectList ([] :: [Filter Command]) []
  liftIO $ print (l :: [Entity Command])

  where
    processRecords records = case records of
      [] -> return ()
      record : rest -> do
        processRecord record
        processRecords rest

    processRecord (C.CommandRecord format time user cmd args) =
      insert $ Command format (formatISO8601Millis time) (T.unpack user) (T.unpack cmd) args' where
      args' = BL.unpack $ encode args

