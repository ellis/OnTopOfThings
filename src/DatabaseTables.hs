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

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Event
  time UTCTime
  user String
  comment String Maybe
  type String
  version Int
  data ByteString
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
  created UTCTime
  creator String
  type String
  status String
  parent String Maybe
  name String Maybe
  title String Maybe
  content String Maybe
  stage String Maybe
  closed String Maybe
  start String Maybe
  end String Maybe
  due String Maybe
  defer String Maybe -- When to next review this item (like GTD tickler)
  --reviewed UTCTime Maybe -- Last time reviewed
  estimate Int Maybe -- time estimate in minutes
  index Int Maybe
  deriving Show
|]

itemEmpty :: String -> UTCTime -> String -> String -> String -> Item
itemEmpty uuid created creator type_ status =
  Item uuid created creator type_ status Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
