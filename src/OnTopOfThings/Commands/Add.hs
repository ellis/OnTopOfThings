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

module OnTopOfThings.Commands.Add
( mode_add
, modeInfo_add
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

modeInfo_add :: ModeInfo
modeInfo_add = (mode_add, ModeRunDB optsProcess1_add optsProcess2_add optsRun_add)

mode_add = Mode
  { modeGroupModes = mempty
  , modeNames = ["add"]
  , modeValue = options_empty "add"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Add a new task"
  , modeHelpSuffix = ["Add a new task and be a dude"]
  , modeArgs = ([flagArg updArgs "TITLE"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (upd "parent") "ID" "reference to parent of this item"
    , flagReq ["id"] (upd "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagReq ["label", "l"] (upd "label") "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (upd "stage") "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["tag", "t"] (upd "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["type"] (upd "type") "TYPE" "list|task. (default=task)"
    , flagHelpSimple updHelp
    ]
  }

-- move the argument to the 'title' field
optsProcess1_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_add opts = do
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  -- replace references with uuids where necessary,
  flags_l_ <- mapM refToUuid (optionsFlags opts)
  return $ getArgs uuid flags_l_
  where
    getArgs :: String -> [Either String (String, String)] -> Validation Options
    getArgs uuid flags_l_ = do
      flags' <- concatEithers1 flags_l_
      -- Get the title field
      let title1_ = M.lookup "title" (optionsMap opts) >>= \x -> x -- title set in flags
      let title2_ = listToMaybe (optionsArgs opts) -- title as first argument
      title <- maybeToEither ["A title must be supplied"] (title1_ `mplus` title2_)
      -- Add 'id' and 'title'
      let flags = [("id", uuid), ("title", title)] ++ flags'
      let options_empty' = options_empty (optionsCmd opts)
      case foldl (\acc_ (name, value) -> acc_ >>= \acc -> upd name value acc) (Right options_empty') flags of
        Left msg -> Left [msg]
        Right x -> Right x

optsProcess2_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_add opts = return (Right opts') where
  defaults = [("type", "task"), ("status", "open"), ("stage", "new")]
  opts' = foldl optionsSetDefault opts defaults where

optsRun_add :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
optsRun_add record opts = do
  case createItem (Command.commandTime record) opts of
    Left msgs -> return (Left msgs)
    Right item -> do
      insert item
      mapM_ (saveProperty (itemUuid item)) (optionsMods opts)
      return $ Right ()

--refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
refToUuid ("parent", ref) = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left "Couldn't find parent ref")
    Just uuid -> return (Right $ ("parent", uuid))
refToUuid x = return $ Right x

--processCommand_mod :: UTCTime -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
--processCommand_mod time args0 = do
--  x <- parseModArgs args0
--  case x of
--    Left msgs -> return (Left msgs)
--    Right args -> do
--      let map = argsToMap args
--      case M.lookup "id" map of
--        Just (Just uuid) -> do
--          entity' <- getBy $ ItemUniqUuid uuid
--          case entity' of
--            Nothing -> return (Left ["Could not find item with given id"])
--            Just entity -> do
--              let item0 = entityVal entity
--              case updateItem time map item0 of
--                Nothing -> return (Left ["Couldn't update item"])
--                Just item -> do
--                  replace (entityKey entity) item
--                  mapM_ (saveProperty uuid) args
--                  return (Right ())
--        Nothing -> return (Left ["Missing `id`"])

itemFields = ["id", "type", "title", "status", "parent", "stage", "label", "index"]

--instance Monad (Either e) where
--  (Left msgs) >>= f = Left msgs
--  (Right x) >>= f = f x
--  return = Right

createItem :: UTCTime -> Options -> Validation Item
createItem _ opts | trace ("createItem: "++(show opts)) False = undefined
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

