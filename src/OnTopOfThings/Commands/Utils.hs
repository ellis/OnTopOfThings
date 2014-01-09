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
, processRefArgsAndFlags
, processRefFlags
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
import OnTopOfThings.Parsers.NumberList


refToUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation String)
refToUuid ref = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left ["Couldn't find ref: "++ref])
    Just uuid -> return (Right uuid)

processRefArgsAndFlags :: Options -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
processRefArgsAndFlags opts0 name = do
  case ids_ of
    Left msgs -> return (Left msgs)
    Right ids -> do
      uuids_ <- mapM refToUuid ids
      return $ do
        uuids <- concatEithersN uuids_
        optionsReplaceParamN name uuids (opts0 { optionsArgs = [] })
  where
    idArgs0 = optionsArgs opts0
    flags0 = optionsFlags opts0
    idFlags0 = catMaybes $ map (\(n, value) -> if n == name then Just value else Nothing) flags0
    ids0 = idArgs0 ++ idFlags0
    ids_ = (concatEithersN $ map parseNumberList ids0) >>= \ll -> (Right $ concat ll)

processRefFlags :: Options -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
processRefFlags opts0 name = do
  case ids_ of
    Left msgs -> return (Left msgs)
    Right ids -> do
      uuids_ <- mapM refToUuid ids
      return $ do
        uuids <- concatEithersN uuids_
        optionsReplaceParamN name uuids (opts0 { optionsArgs = [] })
  where
    flags0 = optionsFlags opts0
    ids0 = catMaybes $ map (\(n, value) -> if n == name then Just value else Nothing) flags0
    ids_ = (concatEithersN $ map parseNumberList ids0) >>= \ll -> (Right $ concat ll)

--instance Monad (Either e) where
--  (Left msgs) >>= f = Left msgs
--  (Right x) >>= f = f x
--  return = Right

--createFolderItems :: UTCTime -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation [Item])
createItem :: UTCTime -> Options -> Validation Item
--createItem _ opts | trace ("Utils.createItem: "++(show opts)) False = undefined
createItem time opts = do
  id <- get "id"
  type_ <- get "type"
  title <- get "title"
  status <- get "status"
  parent <- getMaybe "parent"
  stage <- getMaybe "stage"
  name <- getMaybe "name"
  title <- getMaybe "title"
  content <- getMaybe "content"
  closed <- getMaybeDate "closed"
  start <- getMaybeDate "start"
  end <- getMaybeDate "end"
  due <- getMaybeDate "due"
  review <- getMaybeDate "review"
  -- index
  return $ Item id time "default" type_ status parent name title content stage closed start end due review Nothing
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

updateItem :: UTCTime -> M.Map String (Maybe String) -> Item -> Validation Item
updateItem time map item0 =
  Item <$>
    get "id" itemUuid <*>
    Right (itemCtime item0) <*>
    Right (itemCreator item0) <*>
    get "type" itemType <*>
    get "status" itemStatus <*>
    getMaybe "parent" itemParent <*>
    getMaybe "name" itemName <*>
    getMaybe "title" itemTitle <*>
    getMaybe "content" itemContent <*>
    getMaybe "stage" itemStage <*>
    getMaybeDate "closed" itemClosed <*>
    getMaybeDate "start" itemStart <*>
    getMaybeDate "end" itemEnd <*>
    getMaybeDate "due" itemDue <*>
    getMaybeDate "review" itemReview <*>
    Right (itemIndex item0)
  where
    get :: String -> (Item -> String) -> Validation String
    get name fn = case M.lookup name map of
      Just (Just s) -> Right s
      _ -> Right (fn item0)

    getMaybe :: String -> (Item -> Maybe String) -> Validation (Maybe String)
    getMaybe name fn = case M.lookup name map of
      Just (Just s) -> Right (Just s)
      _ -> Right (fn item0)

    getMaybeDate :: String -> (Item -> Maybe UTCTime) -> Validation (Maybe UTCTime)
    getMaybeDate name fn = case M.lookup name map of
      Just (Just s) -> (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
      _ -> Right (fn item0)

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
