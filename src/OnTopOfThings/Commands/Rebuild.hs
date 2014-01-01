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

module OnTopOfThings.Commands.Rebuild
( modeInfo_rebuild
, mode_rebuild
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Maybe
import Data.Monoid
import Database.Persist.Sqlite
import System.Console.CmdArgs.Explicit
import System.IO
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Command (CommandRecord, loadCommandRecords)
import DatabaseUtils
import Utils
import qualified Database as DB

modeInfo_rebuild :: ModeInfo
modeInfo_rebuild = (mode_rebuild, ModeRunIO optsRun_rebuild)

mode_rebuild = Mode
  { modeGroupModes = mempty
  , modeNames = ["rebuild"]
  , modeValue = options_empty "rebuild"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Rebuild the Sqlite database."
  , modeHelpSuffix = []
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup
    [ flagHelpSimple updHelp
    ]
  }

optsRun_rebuild :: Options -> IO (Validation ())
optsRun_rebuild opts = do
      x <- loadCommandRecords
      case x of
        Left msgs -> return (Left msgs)
        Right records -> do
          mapM_ (putStrLn . show) records
          runSqlite "otot.db" $ do
            DB.databaseInit
            -- 2) convert the command records to and SQL 'command' table
            -- 3) process the 'command' table, producing the 'property' table
            DB.databaseAddRecords records
            DB.databaseUpdateIndexes
            return (Right ())
