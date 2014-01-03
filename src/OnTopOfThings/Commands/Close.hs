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

module OnTopOfThings.Commands.Close
( modeInfo_close
, mode_close
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

modeInfo_close :: ModeInfo
modeInfo_close = (mode_close, ModeRunDB optsProcess1_close optsProcess2_close optsRun_close)

mode_close = Mode
  { modeGroupModes = mempty
  , modeNames = ["close"]
  , modeValue = options_empty "close"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Close an item"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg updArgs "ID"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["id"] (updN "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagHelpSimple updHelp
    ]
  }

-- move the argument to the 'title' field
optsProcess1_close :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_close opts0 = do
  processRefArgsAndFlags opts0 "id"

optsProcess2_close :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_close opts = return (Right opts)

optsRun_close :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
optsRun_close record opts = do
  case M.lookup "id" (optionsParamsN opts) of
    Just uuids -> do
      mapM close (optionsArgs opts)
      return (Right ())
  where
    time = Command.commandTime record
    close uuid = do
      updateWhere [ItemUuid ==. uuid] [ItemStatus =. "closed", ItemClosed =. Just time]
