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
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Commands.Add
import OnTopOfThings.Commands.Close
import OnTopOfThings.Commands.Import (optsToCommandRecord)
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

modeInfo_l :: [ModeInfo]
modeInfo_l =
  [ modeInfo_add
  , modeInfo_close
  ]
modeInfo :: M.Map String ModeInfo
modeInfo = M.fromList $ map (\x@(mode, _) -> (head (modeNames mode), x)) modeInfo_l

mode_root :: Mode Options
mode_root = modes "otot" (options_empty "") "OnTopOfThings for managing lists and tasks"
  (map (\(mode, _) -> mode) modeInfo_l)

optsRun_rebuild :: Options -> IO (Validation ())
optsRun_rebuild opts = do
      x <- loadCommandRecords
      case x of
        Left msgs -> return (Left msgs)
        Right records -> do
          mapM_ (putStrLn . show) records
          runSqlite "otot.db" $ do
            -- Create tables if necessary
            DB.databaseInit
            -- Delete existing rows
            deleteWhere ([] :: [Filter Item])
            deleteWhere ([] :: [Filter Property])
            -- Add the command records and process them
            result <- mapM processRecord records
            let result' = concatEithersN result
            return $ fmap (const ()) result'

processRecord :: CommandRecord -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
processRecord record = do
  let cmd = T.unpack $ Command.commandCmd record
  case M.lookup cmd modeInfo of
    Nothing -> return (Left ["unknown command `"++cmd++"`"])
    Just (mode, run) -> do
      let args = fmap T.unpack (Command.commandArgs record)
      case process mode args of
        Left msg -> return (Left [msg])
        Right opts -> do
          case run of
            ModeRunDB optsProcess1 optsProcess2 optsRunDB -> do
              handleOptions record opts mode optsProcess1 optsProcess2 optsRunDB
              return (Right ())
            ModeRunIO optsRunIO -> do
              x <- liftIO $ optsRunIO opts
              case x of
                Left msgs -> return (Left msgs)
                _ -> return (Right ())

--handleOptions :: CommandRecord -> Mode Options -> 
handleOptions record opts mode optsProcess1 optsProcess2 optsRun = do
  let time = Command.commandTime record
  -- Validate options and add parameters required for the new command record
  opts_ <- optsProcess1 opts
  case opts_ of
    Left msgs -> return (Left msgs)
    Right opts' -> do
      -- Convert Options to CommandRecord
      let record = optsToCommandRecord time "default" opts'
      liftIO $ toStdErr record
      -- TODO: Save CommandRecord to temporary file
      -- TODO: CommandRecord read in from file
      -- TODO: Verify that CommandRecords are equal
      -- Convert CommandRecord to Command
      let command = DB.recordToCommand record
      -- Command saved to DB
      insert command
      -- TODO: Command loaded from DB
      -- TODO: Verify that Commands are equal
      -- TODO: Command converted to a CommandRecord
      -- TODO: Verify that CommandRecords are equal
      -- TODO: CommandRecord converted to Options
      -- TODO: Verify that Options are equal
      -- Options are validated and processed for modification of DB 'item' and 'property' tables
      opts__ <- optsProcess2 opts'
      case opts__ of
        Left msgs -> return (Left msgs)
        Right opts'' -> do
          -- Update items and properties
          x_ <- optsRun record opts''
          case x_ of
            Left msgs -> return (Left msgs)
            Right _ -> return (Right ())

