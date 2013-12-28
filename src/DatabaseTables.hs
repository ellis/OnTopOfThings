{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module DatabaseTables
--( Command(..)
--, Property(..)
--) where
where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Data.Time.Clock (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Command
  format Int
  time UTCTime
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
Item
  ctime UTCTime
  type String
  title String
  status String
  stage String Maybe
  label String Maybe
  index Int Maybe
  deriving Show
|]
