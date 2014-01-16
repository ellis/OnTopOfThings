{-
Copyright (C) 2013,2014  Ellis Whitehead

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

import Control.Monad
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
import qualified OnTopOfThings.Actions.Mod as Mod
import OnTopOfThings.Actions.Mv
import OnTopOfThings.Actions.Run
import OnTopOfThings.Commands.Add
import OnTopOfThings.Commands.Close
import OnTopOfThings.Commands.Delete
import OnTopOfThings.Commands.Import
import OnTopOfThings.Commands.Show
import OnTopOfThings.Commands.Mod
import OnTopOfThings.Commands.Rebuild
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase
import OnTopOfThings.Data.Types
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
  args0 <- getArgs
  let args' = dropWhile (\s -> head s == '-') args0
  case args' of
    [] -> putStrLn "use a command: repl, import"
    (cmd:args) -> do
      case cmd of
        "repl" -> do
          runSqlite "repl.db" $ do
            runMigration migrateAll
          repl ["/"]
        "import" -> do
          let mode = fst modeInfo_import
          let opts_ = process mode args
          case opts_ of
            Left msg -> putStrLn msg
            Right opts -> processOptions opts
        "rebuild" -> do
          let mode = fst modeInfo_rebuild
          let opts_ = process mode args
          case opts_ of
            Left msg -> putStrLn msg
            Right opts -> processOptions opts

repl cwd = do
  putStr "otot> "
  hFlush stdout
  input <- getLine
  time <- getCurrentTime
  let args0 = splitArgs input
  let env0 = Env time "default" cwd
  env1 <- runSqlite "repl.db" $ do
    (env1, result_) <-
      case args0 of
        [] -> return (env0, mempty)
        "cat":args -> do
          case process mode_cat args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode_cat
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionCat) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "ls":args -> do
          case process mode_ls args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode_ls
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionLs) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "close":args -> do
          let mode = OnTopOfThings.Actions.Run.mode_close
          case process mode args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionClose) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "mkdir":args -> do
          case process mode_mkdir args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode_mkdir
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionMkdir) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "mod":args -> do
          let mode = Mod.mode_mod
          case process mode args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionMod) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "mv":args -> do
          let mode = mode_mv
          case process mode args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionMv) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        "newtask":args -> do
          case process mode_newtask args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right action -> do
              if newTaskHelp action
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode_newtask
                  return (env0, mempty)
                else do
                  runAction env0 action
        "show":args -> do
          let mode = fst modeInfo_show
          case process mode args of
            Left msg -> return (env0, ActionResult [] False [] [msg])
            Right opts -> do
              if optionsHelp opts
                then do
                  liftIO $ print $ helpText [] HelpFormatDefault mode
                  return (env0, mempty)
                else do
                  action_ <- actionFromOptions env0 opts
                  case (action_ :: Validation ActionShow) of
                    Left msgs -> return (env0, ActionResult [] False [] msgs)
                    Right action -> do
                      runAction env0 action
        cmd:_ -> do
          --liftIO $ processMode args0
          return (env0, ActionResult [] False [] ["command not found: "++cmd])
    case result_ of
      (ActionResult cards _ warn err) -> do
        liftIO $ mapM_ putStrLn err
        liftIO $ mapM_ putStrLn warn
        fn time cards
    return env1
  repl (envCwdChain env1)
  where
    fn :: UTCTime -> [PatchHunk] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    fn time hunks =
          when (not $ null hunks) $ do
            liftIO $ print hunks
            -- save patch
            chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
            let file = PatchFile1 time "default" Nothing hunks
            liftIO $ saveFileAsJson file chguuid
            -- patch database
            let header = Patch time "default" hunks
            result_ <- patch header
            case result_ of
              Left msgs -> liftIO $ mapM_ putStrLn msgs
              Right _ -> return ()
--            let record = CommandRecord 1 time (T.pack $ envUser env0) (T.pack $ head args0) (map T.pack recordArgs)
--            chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
--            liftIO $ print record
--            -- Save record to disk
--            saveCommandRecord record chguuid
--            runSqlite "repl.db" $ do
--              -- Convert CommandRecord to Command
--              let command = DB.recordToCommand record
--              -- Command saved to DB
--              insert command

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
  processOptions opts

processOptions :: Options -> IO ()
processOptions opts = do
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
