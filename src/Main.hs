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
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import System.IO
import Database.Persist (insert)
import Database.Persist.Sqlite (SqlPersistT, runSqlite)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Import
import List
import Utils
import OnTopOfThings.Commands.Add
import OnTopOfThings.Commands.Close
import OnTopOfThings.Commands.Rebuild
import qualified Database as DB


modeInfo_l :: [ModeInfo]
modeInfo_l =
  [ modeInfo_add
  , modeInfo_close
  , modeInfo_rebuild
  ]
modeInfo = M.fromList modeInfo_l

mode_root = modes "otot" (options_empty "") "OnTopOfThings for managing lists and tasks"
  [mode_add, mode_close, mode_rebuild]


-- 1) load the command records from files
-- 2) convert the command records to and SQL 'command' table
-- 3) process the 'command' table, producing the 'property' table
-- 4) parse the command line and create a new command record
-- 5) convert the new command record to a 'Command' and update the 'property' table
-- 6) save the command record to disk
-- 7) print relevant output
main :: IO ()
main = do
  -- Options read by CmdArgs
  opts <- processArgs mode_root
  toStdErr opts
  -- find info for the chosen command mode
  let cmd = optionsCmd opts
  let (mode, optsProcess1_, optsProcess2_, optsRun_) = case M.lookup cmd modeInfo of { Nothing -> (mode_root, Nothing, Nothing, Nothing); Just x -> x }
  case (cmd, optsProcess1_, optsProcess2_, optsRun_) of
    (_, Just optsProcess1, Just optsProcess2, Just optsRun) -> do
      handleOptions opts mode optsProcess1 optsProcess2 optsRun
      return ()
    ("rebuild", _, _, _) -> do
      -- 1) load the command records from files
      x <- loadCommandRecords
      case x of
        Right records -> do
          mapM_ (putStrLn . show) records
          runSqlite "otot.db" $ do
            DB.databaseInit
            -- 2) convert the command records to and SQL 'command' table
            -- 3) process the 'command' table, producing the 'property' table
            DB.databaseAddRecords records
            DB.databaseUpdateIndexes
            return ()
    _ ->
      print $ helpText [] HelpFormatDefault mode
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

--handleRecord record optsProcess2 = do
--  liftIO $ toStdErr record
--  -- 5) convert the new command record to a 'Command' and update the 'property' table
--  insert record
--  let opts'' = optsProcess2 opts'
--  x <- DB.databaseAddRecord record
--  case x of
--    Left msgs -> return (Left msgs)
--    Right () -> return (Right record)

--handleOptions_add :: Options -> IO ()
--handleOptions_add opts = do
--  -- If help is selected or there are neither arguments nor flags:
--  if (optionsHelp args) || (null (optionsArgs args) && null (optionsFlags args))
--    -- print help for the given mode
--    then print $ helpText [] HelpFormatDefault mode_add
--    else do
--      time <- getCurrentTime
--      record' <- runSqlite "otot.db" $ do
--        -- Validate options and add parameters required for the new command record
--        opts' <- optsProcess1_add opts
--        case opts' of
--          Left msgs -> return (Left msgs)
--          Right opts' -> do
--            let record = optsToCommandRecord time "default" opts'
--            liftIO $ toStdErr record
--            -- 5) convert the new command record to a 'Command' and update the 'property' table
--            insert record
--            let opts'' = 
--            x <- DB.databaseAddRecord record
--            case x of
--              Left msgs -> return (Left msgs)
--              Right () -> return (Right record)
--      case record' of
--        Left msgs -> print msgs
--        Right record -> do
--          chguuid <- U4.nextRandom >>= return . U.toString
--          saveCommandRecord record chguuid
--
--validateArgs_add :: Arguments -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Arguments)
--validateArgs_add args = do
--  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
--  flags' <- mapM refToUuid (argumentsFlags args)
--  return $ getArgs uuid flags'
--  where
--    getArgs :: String -> [Either String (String, String)] -> Validation Arguments
--    getArgs uuid flags' =
--      case concatEithers1 flags' of
--        Left msgs -> Left msgs
--        Right flags'' -> Right $ args { argumentsFlags = flags''' } where
--          flags''' :: [(String, String)]
--          flags''' = ("id", uuid) : flags''
--
----refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
--refToUuid ("parent", ref) = do
--  uuid' <- databaseLookupUuid ref
--  case uuid' of
--    Nothing -> return (Left "Couldn't find parent ref")
--    Just uuid -> return (Right $ ("parent", uuid))
--refToUuid x = return $ Right x
--
--run_add :: Options -> UTCTime -> IO (Validation CommandRecord)
--run_add opts time = do
--
--main' :: IO ()
--main' = do
--  args <- getArgs
--  -- 1) load the command records from files
--  x <- loadCommandRecords
--  case x of
--    Right records -> do
--      mapM_ (putStrLn . show) records
--      runSqlite ":memory:" $ do
--        DB.databaseInit
--        -- 2) convert the command records to and SQL 'command' table
--        -- 3) process the 'command' table, producing the 'property' table
--        DB.databaseAddRecords records
--        DB.databaseUpdateIndexes
--        case args of
--          "add" : args' -> addHandler args'
--          "import" : args' -> liftIO $ importHandler args'
--          "list" : args' -> listHandler args'
--          "mod" : args' -> modHandler args'
--          --"add" : args' -> addHandler args'
--          --"show" : args' -> showHandler args'
--          [] -> do liftIO $ putStrLn "use one of these commands: add, view"
--          _ -> do liftIO $ putStrLn "Unrecognized command"
--    Left msg -> do
--      putStrLn msg
--
--  putStrLn "Done."
--
----addHandler :: [String] -> IO ()
--addHandler args = do
--  time <- liftIO $ getCurrentTime
--  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
--  chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
--  -- 4) parse the command line and create a new command record
--  x <- createAddCommandRecord time "default" uuid args
--  case x of
--    Left msg -> do liftIO $ print msg
--    Right record -> do
--      -- 5) convert the new command record to a 'Command' and update the 'property' table
--      DB.databaseAddRecord record
--      liftIO $ print record
--      liftIO $ saveCommandRecord record chguuid
--  return ()
--
--importHandler args = do
--  processImportCommand (head args)
--
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
optsToCommandRecord :: UTCTime -> T.Text -> Options -> CommandRecord
optsToCommandRecord time user opts = CommandRecord 1 time user (T.pack $ optionsCmd opts) opts'' where
  opts' = reform opts
  opts'' = map T.pack opts'
