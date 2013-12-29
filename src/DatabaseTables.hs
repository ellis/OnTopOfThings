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
  uuid String
  ItemUniqUuid uuid
  ctime UTCTime
  type String
  title String
  status String
  parent String Maybe
  stage String Maybe
  label String Maybe
  index Int Maybe
  deriving Show
|]
