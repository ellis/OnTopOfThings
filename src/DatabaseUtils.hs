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
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
--import Database.Persist.Sqlite
import Database.Esqueleto
import Database.Persist.TH

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import DatabaseTables


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

