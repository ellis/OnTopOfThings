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

module OnTopOfThings.Actions.Cd where

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
import OnTopOfThings.Actions.Utils (itemToAbsPathChain, lookupItem, uuidToItem)
import OnTopOfThings.Commands.Show
--import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase


instance Action ActionCd where
  runAction env action | trace ("runAction") False = undefined
  runAction env action = do
    result <- cd env action
    case result of
      Left msgs -> return (env, ActionResult [] False [] msgs)
      Right env1 -> return (env1, mempty)
  actionFromOptions env opts | trace ("actionFromOptions "++(show opts)) False = undefined
  actionFromOptions env opts = do
    let stage' = M.lookup "stage" (optionsParams1 opts)
    let parentRef_ = M.lookup "parent" (optionsParams1 opts)
    parentUuid_ <- case parentRef_ of
      Nothing -> return (Right Nothing)
      Just parentRef -> do
        parent_ <- lookupItem env parentRef
        case parent_ of
          Left msgs -> return (Left ["cd: folder ‘"++parentRef++"’ is not a directory"])
          -- Destination exists, so move sources to that item
          Right parent -> do
            let parentUuid = itemUuid parent
            return (Right (Just parentUuid))
    case parentUuid_ of
      Left msgs -> return (Left msgs)
      Right parentUuid -> do
        return (Right (ActionCd parentUuid stage'))
  actionToRecordArgs action = Nothing

mode_cd = Mode
  { modeGroupModes = mempty
  , modeNames = ["cd"]
  , modeValue = options_empty "cd"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Change default folder and/or stage."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updCdOption "OPTION"))
  , modeGroupFlags = toGroup
    [ flagNone ["help"] updHelp "display this help and exit"
    ]
  }

updCdOption :: String -> Options -> Either String Options
updCdOption s opts = case s of
  '/':x -> upd1 "parent" x opts
  '+':x -> updN "tag" x opts
  '?':x -> upd1 "stage" x opts
  _ -> Left ("unrecognized option: "++s)

cd :: Env -> ActionCd -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Env)
cd env0 (ActionCd Nothing Nothing) = return $ Right $ env0 { envCwdChain = [], envStage = Nothing }
cd env0 (ActionCd (Just parentUuid) Nothing) = do
  item_ <- uuidToItem parentUuid
  case item_ of
    Left msgs -> return (Left msgs)
    Right item -> do
      chain <- itemToAbsPathChain item
      return $ Right $ env0 { envCwdChain = chain, envStage = Nothing }
