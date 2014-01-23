{-
Copyright (C) 2014  Ellis Whitehead

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

module OnTopOfThings.Actions.Mod where

import Prelude hiding (mod)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone)
import Data.Time.ISO8601
import Database.Persist (insert)
import Database.Persist.Sqlite
import Debug.Trace
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories)
import System.IO
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.Yaml as Yaml

import Args
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Parsers.NumberList
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env
import OnTopOfThings.Actions.Utils (lookupItem)
--import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase
import OnTopOfThings.Data.Time
import OnTopOfThings.Data.Types


instance Action ActionMod where
  runAction env action | trace ("runAction") False = undefined
  runAction env action = mod env action >>= \result -> return (env, result)
  actionFromOptions env opts | trace ("actionFromOptions "++(show opts)) False = undefined
  actionFromOptions env opts = do
    tz <- liftIO $ getCurrentTimeZone
    items_ <- case optionsArgs opts of
      [] -> return (Left ["mod: missing file operand", "Try 'mod --help' for more information."])
      args -> do
        case concatEithersN (map parseNumberList args) of
          Left msgs -> return (Left msgs)
          Right refs' -> do
            let refs = concat refs'
            uuids_ <- mapM (lookupItem env) refs
            case concatEithersN uuids_ of
              Left msgs -> return (Left msgs)
              Right uuids -> return (Right uuids)
    parentItem_ <- case M.lookup "parent" (optionsParams1 opts) of
      Nothing -> return (Right Nothing)
      Just ref -> do
        item_ <- lookupItem env ref
        case item_ of
          Left msgs -> return (Left msgs)
          Right item -> return (Right (Just item))
    closed_ <- case M.lookup "closed" (optionsParams1 opts) of
      Nothing -> return (Right Nothing)
      Just s -> case parseISO8601 s of
        Nothing -> return (Left ["Expected ISO8601 format for `closed` time: "++s])
        Just utc -> return (Right (Just utc))
    start_ <- case M.lookup "start" (optionsParams1 opts) of
      Nothing -> return (Right Nothing)
      Just s -> return (parseTime' tz s >>= \time -> Right (Just time))
    end_ <- case M.lookup "end" (optionsParams1 opts) of
      Nothing -> return (Right Nothing)
      Just s -> return (parseTime' tz s >>= \time -> Right (Just time))
    return $ do
      items <- items_
      let uuids = map itemUuid items
      parentItem <- parentItem_
      let parentUuid = parentItem  >>= \item -> Just (itemUuid item)
      closed <- closed_
      start <- start_
      end <- end_
      return (ActionMod
                { modUuids = uuids
                , modType = M.lookup "type" (optionsParams1 opts)
                , modStatus = M.lookup "status" (optionsParams1 opts)
                , modParentUuid = parentUuid
                , modName = M.lookup "name" (optionsParams1 opts)
                , modTitle = M.lookup "title" (optionsParams1 opts)
                , modContent = M.lookup "content" (optionsParams1 opts)
                , modStage = M.lookup "stage" (optionsParams1 opts)
                , modClosed = closed
                , modStart = start
                , modEnd = end
                , modTag = M.lookup "tag" (optionsParamsM opts)
                , modOptions = opts
                })
  actionToRecordArgs action = Nothing

mode_mod = Mode
  { modeGroupModes = mempty
  , modeNames = ["mod"]
  , modeValue = options_empty "mod"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Modify an existing item"
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "ID"))
  , modeGroupFlags = toGroup
    [ flagReq ["type"] (upd1 "type") "TYPE" "list|task. (default=task)"
    , flagReq ["status"] (upd1 "status") "STATUS" "open|closed|deleted"
    , flagReq ["parent", "p"] (upd1 "parent") "ID" "reference to parent of this item"
    , flagReq ["name", "n"] (upd1 "name") "NAME" "A filename for this item."
    , flagReq ["title"] (upd1 "title") "TITLE" "Title of the item."
    , flagReq ["content"] (upd1 "content") "CONTENT" "Content of the item."
    , flagReq ["stage", "s"] (upd1 "stage") "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["closed"] (upd1 "closed") "TIME" "Time that this item was closed."
    , flagReq ["start"] (upd1 "start") "TIME" "Start time for this event."
    , flagReq ["end"] (upd1 "end") "TIME" "End time for this event."
    , flagReq ["tag", "t"] (updM "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagHelpSimple updHelp
    ]
  }

mod :: Env -> ActionMod -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
mod env action | trace ("mod "++(show action)) False = undefined
mod env action = do
  let diffs = concat $ catMaybes
        [ get "type" modType
        , get "status" modStatus
        , get "parent" modParentUuid
        , get "name" modName
        , get "title" modTitle
        , get "content" modContent
        , get "stage" modStage
        , getUTC "closed" modClosed
        , getTime "start" modStart
        , getTime "end" modEnd
        , (modTag action) >>= \l -> Just (map modToDiff l)
        ]
  let hunk = PatchHunk (modUuids action) diffs
  return (ActionResult [hunk] False [] [])
  where
    get :: String -> (ActionMod -> Maybe String) -> Maybe [Diff]
    get name fn = (fn action) >>= \x -> Just [DiffEqual name x]
    getUTC :: String -> (ActionMod -> Maybe UTCTime) -> Maybe [Diff]
    getUTC name fn = (fn action) >>= \x -> Just [DiffEqual name (formatISO8601 x)]
    getTime :: String -> (ActionMod -> Maybe Time) -> Maybe [Diff]
    getTime name fn = (fn action) >>= \x -> Just [DiffEqual name (formatTime' x)]

modToDiff :: Mod -> Diff
modToDiff mod = case mod of
  ModNull name -> DiffNull name
  ModEqual n v -> DiffEqual n v
  ModAdd n v -> DiffAdd n v
  ModUnset n -> DiffUnset n
  ModRemove n v -> DiffRemove n v
