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
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import System.IO
import Database.Persist.Sqlite (SqlPersistT, runSqlite)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Add
import Args
import Command
import DatabaseUtils
import Import
import List
import Utils
import qualified Database as DB


-- 1) load the command records from files
-- 2) convert the command records to and SQL 'command' table
-- 3) process the 'command' table, producing the 'property' table
-- 4) parse the command line and create a new command record
-- 5) convert the new command record to a 'Command' and update the 'property' table
-- 6) save the command record to disk
-- 7) print relevant output
main :: IO ()
main = do
  args <- processArgs arguments
  toStdErr args
  case argumentsCmd args of
    "" ->
      print $ helpText [] HelpFormatDefault arguments
    "add" -> do
      if (argumentsHelp args) || (null (argumentsArgs args) && null (argumentsFlags args))
        then print $ helpText [] HelpFormatDefault arguments_add
        else do
          time <- getCurrentTime
          record' <- run_add args time
          case record' of
            Left msgs -> print msgs
            Right record -> do
              chguuid <- U4.nextRandom >>= return . U.toString
              saveCommandRecord record chguuid
    "close" -> return ()
    "rebuild" -> do
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

toStdErr x = hPutStrLn stderr $ show x

validateArgs_add :: Arguments -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Arguments)
validateArgs_add args = do
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  flags' <- mapM refToUuid (argumentsFlags args)
  return $ getArgs uuid flags'
  where
    getArgs :: String -> [Either String (String, String)] -> Validation Arguments
    getArgs uuid flags' =
      case concatEithers1 flags' of
        Left msgs -> Left msgs
        Right flags'' -> Right $ args { argumentsFlags = flags''' } where
          flags''' :: [(String, String)]
          flags''' = ("id", uuid) : flags''

--refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
refToUuid ("parent", ref) = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left "Couldn't find parent ref")
    Just uuid -> return (Right $ ("parent", uuid))
refToUuid x = return $ Right x

run_add :: Arguments -> UTCTime -> IO (Validation CommandRecord)
run_add args time = do
  runSqlite "otot.db" $ do
    x <- validateArgs_add args
    -- 4) parse the command line and create a new command record
    case x of
      Left msgs -> return (Left msgs)
      Right args' -> do
        let record = argumentsToRecord time "default" args'
        liftIO $ toStdErr record
        -- 5) convert the new command record to a 'Command' and update the 'property' table
        x <- DB.databaseAddRecord record
        case x of
          Left msgs -> return (Left msgs)
          Right () -> return (Right record)

main' :: IO ()
main' = do
  args <- getArgs
  -- 1) load the command records from files
  x <- loadCommandRecords
  case x of
    Right records -> do
      mapM_ (putStrLn . show) records
      runSqlite ":memory:" $ do
        DB.databaseInit
        -- 2) convert the command records to and SQL 'command' table
        -- 3) process the 'command' table, producing the 'property' table
        DB.databaseAddRecords records
        DB.databaseUpdateIndexes
        case args of
          "add" : args' -> addHandler args'
          "import" : args' -> liftIO $ importHandler args'
          "list" : args' -> listHandler args'
          "mod" : args' -> modHandler args'
          --"add" : args' -> addHandler args'
          --"show" : args' -> showHandler args'
          [] -> do liftIO $ putStrLn "use one of these commands: add, view"
          _ -> do liftIO $ putStrLn "Unrecognized command"
    Left msg -> do
      putStrLn msg

  putStrLn "Done."

--addHandler :: [String] -> IO ()
addHandler args = do
  time <- liftIO $ getCurrentTime
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
  -- 4) parse the command line and create a new command record
  x <- createAddCommandRecord time "default" uuid args
  case x of
    Left msg -> do liftIO $ print msg
    Right record -> do
      -- 5) convert the new command record to a 'Command' and update the 'property' table
      DB.databaseAddRecord record
      liftIO $ print record
      liftIO $ saveCommandRecord record chguuid
  return ()

importHandler args = do
  processImportCommand (head args)

modHandler args = do
  time <- liftIO $ getCurrentTime
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  chguuid <- liftIO $ U4.nextRandom >>= return . U.toString
  -- 4) parse the command line and create a new command record
  x <- createModCommandRecord time "default" uuid args
  case x of
    Left msg -> do liftIO $ print msg
    Right record -> do
      -- 5) convert the new command record to a 'Command' and update the 'property' table
      DB.databaseAddRecord record
      liftIO $ print record
      liftIO $ saveCommandRecord record chguuid
  return ()

argumentsToRecord :: UTCTime -> Text -> Arguments -> CommandRecord
argumentsToRecord time user args = CommandRecord 1 time user (pack $ argumentsCmd args) args'' where
  args' = reform args
  args'' = map pack args'
