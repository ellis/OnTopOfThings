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

{-# LANGUAGE OverloadedStrings #-}

module OnTopOfThings.Commands.Utils
( refToUuid
, createItem
, updateItem
, saveProperty
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (mplus)
import Data.Maybe
import Data.Monoid
import Debug.Trace
import System.Console.CmdArgs.Explicit
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
-- Database-related imports
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite
-- Time-related imports
import Data.Time.Clock
import Data.Time.Format
import Data.Time.ISO8601
import System.Locale (defaultTimeLocale)

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils


refToUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation String)
refToUuid ref = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left ["Couldn't find ref: "++ref])
    Just uuid -> return (Right uuid)

--instance Monad (Either e) where
--  (Left msgs) >>= f = Left msgs
--  (Right x) >>= f = f x
--  return = Right

createItem :: UTCTime -> Options -> Validation Item
createItem _ opts | trace ("Utils.createItem: "++(show opts)) False = undefined
createItem time opts = do
  id <- get "id"
  type_ <- get "type"
  title <- get "title"
  status <- get "status"
  parent <- getMaybe "parent"
  stage <- getMaybe "stage"
  label <- getMaybe "label"
  -- index
  closed <- getMaybeDate "closed"
  start <- getMaybeDate "start"
  end <- getMaybeDate "end"
  due <- getMaybeDate "due"
  review <- getMaybeDate "review"
  return $ Item id time type_ title status parent stage label Nothing closed start end due review
  where
    map = optionsMap opts
    get name = case M.lookup name map of
      Just (Just x) -> Right x
      _ -> Left ["missing value for `" ++ name ++ "`"]
    getMaybe name = case M.lookup name map of
      Just (Just s) -> Right (Just s)
      _ -> Right Nothing
    getMaybeDate :: String -> Validation (Maybe UTCTime)
    getMaybeDate name = case M.lookup name map of
      Just (Just s) ->
        (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
      _ -> Right Nothing

updateItem :: UTCTime -> M.Map String (Maybe String) -> Item -> Maybe Item
updateItem time map item0 =
  Item <$>
    get "id" itemUuid <*>
    Just (itemCtime item0) <*>
    get "type" itemType <*>
    get "title" itemTitle <*>
    get "status" itemStatus <*>
    getMaybe "parent" itemParent <*>
    getMaybe "stage" itemStage <*>
    getMaybe "label" itemLabel <*>
    Just (itemIndex item0) <*>
    getMaybeDate "closed" itemClosed <*>
    getMaybeDate "start" itemStart <*>
    getMaybeDate "end" itemEnd <*>
    getMaybeDate "due" itemDue <*>
    getMaybeDate "review" itemReview
  where
    get :: String -> (Item -> String) -> Maybe String
    get name fn = case M.lookup name map of
      Just (Just s) -> Just s
      _ -> Just (fn item0)

    getMaybe :: String -> (Item -> Maybe String) -> Maybe (Maybe String)
    getMaybe name fn = case M.lookup name map of
      Just (Just s) -> Just (Just s)
      _ -> Just (fn item0)

    getMaybeDate :: String -> (Item -> Maybe UTCTime) -> Maybe (Maybe UTCTime)
    getMaybeDate name fn = case M.lookup name map of
      Just (Just s) -> case parseTime defaultTimeLocale "%Y%m%dT%H%M%S" s of
        Just time -> Just (Just time)
        Nothing -> Nothing
      _ -> Just (fn item0)

itemFields = ["id", "type", "title", "status", "parent", "stage", "label", "index", "closed", "start", "end", "due", "review"]

saveProperty :: String -> Mod -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
saveProperty uuid (ModEqual name value) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
    insert $ Property "item" uuid name value
    return ()
saveProperty uuid (ModUnset name) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
    return ()
saveProperty uuid (ModAdd name value) = do
  if elem name itemFields then return () else do
    insert $ Property "item" uuid name value
    return ()
saveProperty uuid (ModRemove name value) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name, PropertyValue ==. value]
    return ()
saveProperty _ arg = do
  liftIO $ print $ "Don't know how to handle arg: " ++ (show arg)