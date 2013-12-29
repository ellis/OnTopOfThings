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
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
import Database.Esqueleto
import Database.Persist.TH
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Time.Clock (UTCTime)
import DatabaseTables


--databaseLookupUuid :: PersistQuery m => String -> m (Maybe String)
databaseLookupUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe String)
databaseLookupUuid ref = do
  a <- fn (\t -> t ^. ItemUuid ==. val ref)
  case a of
    Just uuid -> return a
    _ -> do
      let n' = readMaybe ref :: Maybe Int
      b <- case n' of
        Nothing -> return Nothing
        Just n -> fn (\t -> t ^. ItemIndex ==. val (Just n))
      case b of
        Just uuid -> return b
        _ -> do
          c <- fn (\t -> t ^. ItemLabel ==. val (Just ref))
          return c
  where
    fn expr = do
      l <- select $ from $ \t -> do
        where_ (expr t)
        limit 2
        return (t ^. ItemUuid)
      case l of
        [Value uuid] -> return $ Just uuid
        _ -> return $ Nothing
