{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

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


main :: IO ()
main = do
  x <- loadCommandRecords
  case x of
    Right records -> do
      mapM_ (putStrLn . show) records
      DB.processCommandRecords records
    Left msg -> do
      putStrLn msg

  args <- getArgs
  case args of
    "add" : args' -> addHandler args'
    --"show" : args' -> showHandler args'
    [] -> do putStrLn "use one of these commands: add, view"
    _ -> do putStrLn "Unrecognized command"

  putStrLn "Done."

addHandler :: [String] -> IO ()
addHandler args = do
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
