-- Once I make this a project, then import Text.ShellEscape for printing the shell commands
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import System.Process -- want rawSystem
import Data.List
import Data.Char
import Data.Maybe -- want catMaybes
import Control.Monad
import Text.Printf

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

-- commands: add, view, report

main = do
  args <- getArgs
  case args of
    "add" : args' -> addHandler args'
    "show" : args' -> showHandler args'
    [] -> do putStrLn "use one of these commands: add, view"
    _ -> do putStrLn "Unrecognized command"

  putStrLn "Done."

addHandler :: [String] -> IO ()
addHandler args =
  case getOpt Permute addOptions args of
    (actions, nonOptions, []) -> do
      opts <- foldl (>>=) (return defaultAddOptions) actions
      case nonOptions of
        [] -> do
          hPutStrLn stderr "You must specify a title for the task."
          exitWith (ExitFailure 1)
        words -> do
          let title = unwords words
          let cmds = makeCmd opts title
          putStrLn $ show cmds
          --mapM_ (\(cmd, args) -> rawSystem cmd args) cmds
    (_, _, errors) -> do
      hPutStrLn stderr (concat errors ++ usageInfo ("ft add:") addOptions)
      exitWith (ExitFailure 1)
  where
    makeCmd :: AddOptions -> String -> [(String, [String])]
    makeCmd opts title = [("fossil", "ticket":"add":args)] where
      maybes :: [Maybe [String]]
      maybes =
        [ Just ["type", addOptType opts]
        , Just ["title", title]
        , fmap (\x -> ["parent", x]) (addOptParent opts)
        , fmap (\x -> ["stage", x]) (addOptStage opts)
        ]
      args = concat $ catMaybes maybes


showHandler :: [String] -> IO ()
showHandler args =
  case getOpt Permute showOptions args of
    (actions, nonOptions, []) -> do
      opts <- foldl (>>=) (return defaultShowOptions) actions
      case nonOptions of
        [] -> do
          hPutStrLn stderr "You must specify a title for the task."
          exitWith (ExitFailure 1)
        words -> do
          let title = unwords words
          let cmds = makeCmd opts title
          putStrLn $ show cmds
          mapM_ (\(cmd, args) -> rawSystem cmd args) cmds
    (_, _, errors) -> do
      hPutStrLn stderr (concat errors ++ usageInfo ("ft show:") showOptions)
      exitWith (ExitFailure 1)
  where
    makeCmd :: ShowOptions -> String -> [(String, [String])]
    makeCmd opts title = map (\q -> ("fossil", ["sqlite3", "../db.fossil", q])) queries where
      queries :: [String]
      queries =
        [ "select * from ticket where tkt_uuid = '" ++ title ++ "'"
        , "select * from ticket where title = '" ++ title ++ "'"
        , "select * from ticket where parent = '" ++ title ++ "'"
        ]

