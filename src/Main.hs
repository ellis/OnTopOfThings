{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Database.Persist.Sqlite (runSqlite)

import Add
import Change
import Command
import qualified Database as DB

-- ADD

data AddOptions = AddOptions
  { addOptVerbose :: Bool
  , addOptType :: String
  , addOptParent :: Maybe String
  , addOptStage :: Maybe String
  } deriving Show

defaultAddOptions = AddOptions
  { addOptVerbose = False
  , addOptType = "task"
  , addOptParent = Nothing
  , addOptStage = Just "inbox"
  }

addOptions :: [OptDescr (AddOptions -> IO AddOptions)]
addOptions =
  [ Option ['v'] ["verbose"]
    (NoArg (\opts -> return opts { addOptVerbose = True }))
    "verbose output"
  , Option [] ["list"]
    (NoArg (\opts -> return opts { addOptType = "list" }))
    "add a new list"
  , Option ['p'] ["parent"]
    (ReqArg (\uuid opts -> return opts { addOptParent = Just uuid }) "UUID")
    "specify parent UUID of item"
  , Option ['h'] ["help"]
    (NoArg (\_ -> do
      prg <- getProgName
      hPutStrLn stderr (usageInfo (prg++" add [OPTION...] title") addOptions)
      exitWith ExitSuccess))
    "Show help"
  ]

-- SHOW

data ShowOptions = ShowOptions
  { showOptVerbose :: Bool
  , showOptType :: String
  , showOptParent :: Maybe String
  } deriving Show

defaultShowOptions = ShowOptions
  { showOptVerbose = False
  , showOptType = "task"
  , showOptParent = Nothing
  }

showOptions :: [OptDescr (ShowOptions -> IO ShowOptions)]
showOptions =
  [ Option ['v'] ["verbose"]
    (NoArg (\opts -> return opts { showOptVerbose = True }))
    "verbose output"
  , Option [] ["list"]
    (NoArg (\opts -> return opts { showOptType = "list" }))
    "show a new list"
  , Option ['p'] ["parent"]
    (ReqArg (\uuid opts -> return opts { showOptParent = Just uuid }) "UUID")
    "specify parent UUID of item"
  , Option ['h'] ["help"]
    (NoArg (\_ -> do
      prg <- getProgName
      hPutStrLn stderr (usageInfo (prg++" show [OPTION...] title") showOptions)
      exitWith ExitSuccess))
    "Show help"
  ]


-- 1) load the command records from files
-- 2) convert the command records to and SQL 'command' table
-- 3) process the 'command' table, producing the 'property' table
-- 4) parse the command line and create a new command record
-- 5) convert the new command record to a 'Command' and update the 'property' table
-- 6) save the command record to disk
-- 7) print relevant output
main :: IO ()
main = do
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
        case args of
          "add" : args' -> addHandler args'
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
  return ()

addHandler' :: [String] -> IO ()
addHandler' args = do
  time <- getCurrentTime
  uuid <- U4.nextRandom >>= return . U.toString
  chguuid <- U4.nextRandom >>= return . U.toString
  case getOpt Permute addOptions args of
    (actions, nonOptions, []) -> do
      opts <- foldl (>>=) (return defaultAddOptions) actions
      case nonOptions of
        [] -> do
          hPutStrLn stderr "You must specify a title for the task."
          exitWith (ExitFailure 1)
        words -> do
          let title = unwords words
          let record = makeCmd opts title time uuid
          --putStrLn $ show record
          saveChangeRecord record chguuid
    (_, _, errors) -> do
      hPutStrLn stderr (concat errors ++ usageInfo ("ft add:") addOptions)
      exitWith (ExitFailure 1)
  where
    makeCmd :: AddOptions -> String -> UTCTime -> String -> ChangeRecord
    makeCmd opts title time uuid = ChangeRecord 1 time [entity] where
      entity = ChangeEntity "item" (pack uuid) l
      l = catMaybes
        [ Just $ ChangeProperty "type" "=" (pack $ addOptType opts)
        , Just $ ChangeProperty "title" "=" $ pack title
        , fmap (\x -> ChangeProperty "parent" "=" $ pack x) (addOptParent opts)
        , fmap (\x -> ChangeProperty "stage" "=" $ pack x) (addOptStage opts)
        ]
