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

module OnTopOfThings.Actions.Mv where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
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
import OnTopOfThings.Actions.Run (lookupItem)
import OnTopOfThings.Commands.Show
import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase


instance Action ActionMv where
  runAction env action | trace ("runAction") False = undefined
  runAction env action = mv env action >>= \result -> return (env, result)
  actionFromOptions env opts | trace ("actionFromOptions "++(show opts)) False = undefined
  actionFromOptions env opts = do
    case optionsArgs opts of
      [] -> return (Left ["mv: missing file operand", "Try 'mv --help' for more information."])
      s:[] -> return (Left ["mv: missing destination file operand after ‘"++s++"’", "Try 'mv --help' for more information."])
      args -> do
        -- Check for source items
        let sourceRefs = init args
        sources_ <- mapM (lookupItem env) sourceRefs
        case concatEithersN sources_ of
          Left msgs -> return (Left msgs)
          Right sources -> do
            let sourceUuids = map (itemUuid) sources
            -- Check for existence of destination item
            let destRef = last args
            dest_ <- lookupItem env destRef
            case dest_ of
              -- Destination doesn't exist, so maybe rename
              Left msgs ->
                case sourceUuids of
                  sourceUuid:[] ->
                    -- TODO: splitDirectories destRef, in order to put in correct directory and give correct name
                    return (Right (ActionMvToRename sourceUuid destRef))
                  _ -> return (Left ["mv: target ‘"++destRef++"’ is not a directory"])
              -- Destination exists, so move sources to that item
              Right dest -> do
                let destUuid = itemUuid dest
                return (Right (ActionMvToDirectory sourceUuids destUuid))
  actionToRecordArgs action = Nothing

mode_mv = Mode
  { modeGroupModes = mempty
  , modeNames = ["mv"]
  , modeValue = options_empty "mv"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Rename SOURCE to DEST, or move SOURCE(s) to DEST."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "DEST"))
  , modeGroupFlags = toGroup
    [ flagNone ["help"] updHelp "display this help and exit"
    ]
  }

mv :: Env -> ActionMv -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
mv env action | trace ("mv "++(show action)) False = undefined
mv env (ActionMvToDirectory sourceUuids destUuid) = do
  let diffs =
        [ DiffEqual "parent" destUuid
        ]
  let hunk = PatchHunk sourceUuids diffs
  return (ActionResult [hunk] False [] [])
mv env (ActionMvToRename sourceUuid destName) = do
  let diffs =
        [ DiffEqual "name" destName
        ]
  let hunk = PatchHunk [sourceUuid] diffs
  return (ActionResult [hunk] False [] [])
