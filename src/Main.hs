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
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import System.IO
import Database.Persist.Sqlite (runSqlite)

import Add
import Args
import Command
import Import
import List
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
  xs <- processArgs arguments
  if True -- ("help","") `elem` xs
    then print $ helpText [] HelpFormatDefault arguments
    else print $ show xs
  putStrLn . show $ xs
  --mapM_ (putStrLn . show) xs
  return ()

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
