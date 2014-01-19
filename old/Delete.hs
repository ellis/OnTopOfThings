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

module OnTopOfThings.Commands.Delete
( modeInfo_delete
, mode_delete
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
import OnTopOfThings.Commands.Utils

modeInfo_delete :: ModeInfo
modeInfo_delete = (mode_delete, ModeRunDB optsProcess1_delete optsProcess2_delete optsRun_delete)

mode_delete = Mode
  { modeGroupModes = mempty
  , modeNames = ["delete"]
  , modeValue = options_empty "delete"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Delete an item"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg updArgs "ID"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["id"] (updN "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagHelpSimple updHelp
    ]
  }

-- move the argument to the 'title' field
optsProcess1_delete :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_delete opts0 = do
  processRefArgsAndFlags opts0 "id"

optsProcess2_delete :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_delete opts = return (Right opts)

optsRun_delete :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
optsRun_delete record opts = do
  mapM delete uuids
  return (Right ())
  where
    uuids = fromMaybe [] $ M.lookup "id" (optionsParamsN opts)
    time = Command.commandTime record
    delete uuid = do
      updateWhere [ItemUuid ==. uuid] [ItemStatus =. "deleted", ItemClosed =. Just time]
