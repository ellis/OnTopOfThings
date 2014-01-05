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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import System.FilePath.Posix (splitDirectories)
import System.IO
import Database.Persist (insert)
--import Database.Persist.Sqlite (SqlPersistT, runSqlite)
import Database.Persist.Sqlite
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env
import OnTopOfThings.Actions.Run
import OnTopOfThings.Commands.Add
import OnTopOfThings.Commands.Close
import OnTopOfThings.Commands.Delete
import OnTopOfThings.Commands.Import
import OnTopOfThings.Commands.Show
import OnTopOfThings.Commands.Mod
import OnTopOfThings.Commands.Rebuild
import OnTopOfThings.Parsers.NumberList
import qualified Database as DB


modeInfo_l :: [ModeInfo]
modeInfo_l =
  [ modeInfo_add
  , modeInfo_close
  , modeInfo_delete
  , modeInfo_import
  , modeInfo_mod
  , modeInfo_rebuild
  , modeInfo_show
  ]
modeInfo :: M.Map String ModeInfo
modeInfo = M.fromList $ map (\x@(mode, _) -> (head (modeNames mode), x)) modeInfo_l

mode_root :: Mode Options
mode_root = modes "otot" (options_empty "") "OnTopOfThings for managing lists and tasks"
  (map (\(mode, _) -> mode) modeInfo_l)


-- 1) load the command records from files
-- 2) convert the command records to and SQL 'command' table
-- 3) process the 'command' table, producing the 'property' table
-- 4) parse the command line and create a new command record
-- 5) convert the new command record to a 'Command' and update the 'property' table
-- 6) save the command record to disk
-- 7) print relevant output
main :: IO ()
main = do
  runSqlite "repl.db" $ do
    runMigration migrateAll
  repl ["/"]

repl cwd = do
  putStr "otot> "
  hFlush stdout
  input <- getLine
  time <- getCurrentTime
  let args0 = splitArgs input
  let env0 = Env time "default" cwd
  (env1, recordArgs, result_) <- runSqlite "repl.db" $ do
    case args0 of
      [] -> return (env0, [], mempty)
      "ls":args -> do
        case process mode_ls args of
          Left msg -> return (env0, [], ActionResult False False [] [msg])
          Right opts -> do
            if optionsHelp opts
              then do
                liftIO $ print $ helpText [] HelpFormatDefault mode_ls
                return (env0, [], mempty)
              else do
                action_ <- actionFromOptions opts
                case (action_ :: Validation ActionLs) of
                  Left msgs -> return (env0, [], ActionResult False False [] msgs)
                  Right action -> do
                    result_ <- runAction env0 action
                    case result_ of
                      (env', action', result') -> return (env', Just action', result')
      "mkdir":args -> do
        case process mode_mkdir args of
          Left msg -> return (env0, Nothing, ActionResult False False [] [msg])
          Right opts -> do
            if optionsHelp opts
              then do
                liftIO $ print $ helpText [] HelpFormatDefault mode_mkdir
                return (env0, Nothing, mempty)
              else do
                action_ <- actionFromOptions opts
                case (action_ :: Validation ActionMkdir) of
                  Left msgs -> return (env0, ActionResult False False [] msgs)
                  Right action -> do
                    result_ <- runAction env0 action
                    case result_ of
                      (env', action', result') -> return (env', Just action', result')
      "newtask":args -> do
        case process mode_newtask args of
          Left msg -> return (env0, ActionResult False False [] [msg])
          Right action -> do
            if newTaskHelp action
              then do
                liftIO $ print $ helpText [] HelpFormatDefault mode_newtask
                return (env0, mempty)
              else do
                result_ <- runAction env0 action
                case result_ of
                  (env', action', result') -> return (env', Just (actionToRecordArgs action'), result')
      cmd:_ -> do
        liftIO $ processMode args0
        return (env0, Nothing, ActionResult False False [] ["command not found: "++cmd])
  case result_ of
    (ActionResult change rollback warn err) -> do
      liftIO $ mapM_ putStrLn err
      liftIO $ mapM_ putStrLn warn
      if rollback
        then liftIO $ putStrLn "ERROR: NEED TO `rebuild`"
        else if change
          then do
            case action_ of
              Nothing -> return ()
              Just action ->
                case actionToRecordArgs action of
                  Nothing -> return ()
                  Just args -> do
                    let record = CommandRecord 1 time (T.pack $ envUser env0) (T.pack $ head args0) (map T.pack args)
                    liftIO $ print record
          else
            return ()
      -- TODO: if change && not rollback: create and save command record
      -- TODO: if change && rollback: rebuild the database
      --liftIO $ print result_
  repl (envCwdChain env1)

-------------------------------------------------------------

mainOld :: IO ()
mainOld = do
  args <- getArgs
  processMode args

processMode :: [String] -> IO ()
processMode args = do
  -- Options read by CmdArgs
  let opts = processValue mode_root args
  --toStdErr opts
  -- find info for the chosen command mode
  let cmd = optionsCmd opts
  case M.lookup cmd modeInfo of
    Nothing -> do
      print $ helpText [] HelpFormatDefault mode_root
    Just (mode, run) -> do
      -- Is help requested?
      if optionsHelp opts
        -- print help for the given mode
        then print $ helpText [] HelpFormatDefault mode
        else
          case run of
            ModeRunDB optsProcess1 optsProcess2 optsRunDB -> do
              handleOptions opts mode optsProcess1 optsProcess2 optsRunDB
              return ()
            ModeRunIO optsRunIO -> do
              x <- optsRunIO opts
              case x of
                Left msgs -> mapM_ putStrLn msgs
                _ -> return ()
  return ()

--handleOptions :: Options -> Mode -> (IO ()
handleOptions opts mode optsProcess1 optsProcess2 optsRun = do
  -- If help is selected or there are neither arguments nor flags:
  if (optionsHelp opts) || (null (optionsArgs opts) && null (optionsFlags opts))
    -- print help for the given mode
    then print $ helpText [] HelpFormatDefault mode
    else do
      time <- getCurrentTime
      chguuid <- U4.nextRandom >>= return . U.toString
      record' <- runSqlite "otot.db" $ do
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
                  Right _ -> return (Right record)
      case record' of
        Left msgs -> mapM_ putStrLn msgs
        Right record -> do
          saveCommandRecord record chguuid
          return ()

--modHandler args = do
--  time <- liftIO $ getCurrentTime
--  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
--  chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
--  -- 4) parse the command line and create a new command record
--  x <- createModCommandRecord time "default" uuid args
--  case x of
--    Left msg -> do liftIO $ print msg
--    Right record -> do
--      -- 5) convert the new command record to a 'Command' and update the 'property' table
--      DB.databaseAddRecord record
--      liftIO $ print record
--      liftIO $ saveCommandRecord record chguuid
--  return ()
--
