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

module DatabaseUtils
( databaseLookupUuid
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Generics.Aliases (orElse)
import Data.Time.ISO8601 (formatISO8601Millis)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Debug.Trace
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Time.Clock (UTCTime)
import DatabaseTables


--databaseLookupUuid :: PersistQuery m => String -> m (Maybe String)
databaseLookupUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String)
--databaseLookupUuid ref | trace ("databaseLookupUuid: " ++ ref) False = undefined
databaseLookupUuid ref = do
  a <- fn [ItemUuid ==. ref]
  case a of
    Just uuid -> return a
    _ -> do
      let n' = readMaybe ref :: Maybe Int
      b <- case n' of
        Nothing -> return Nothing
        Just n -> fn [ItemIndex ==. (Just n)]
      case b of
        Just uuid -> return b
        _ -> do
          c <- fn [ItemName ==. Just ref]
          return c
  where
    fn :: [Filter Item] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String)
    fn filters = do
      --if ref == "fde44ebb-8c4d-44f2-b2dd-a228d9530030" then (liftIO . print) "databaseLookupUuid" else return ()
      l <- selectList filters [LimitTo 2]
      case l of
        entity:[] -> return $ Just (itemUuid $ entityVal entity)
        _ -> return $ Nothing
