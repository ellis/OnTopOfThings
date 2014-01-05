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
  repl

repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let args0 = splitArgs input
  time <- getCurrentTime
  record_ <- runSqlite "repl.db" $ do
    case args0 of
      [] -> return (Nothing :: Maybe CommandRecord, [], [])
      "ls":args -> do
        let cmd = CommandLs args
        result <- ls time "default" ["/"] cmd
        case result of
          Left msgs -> return (Nothing, [], msgs)
          Right _ -> return (Nothing, [], [])
      "mkdir":args -> do
        let cmd = CommandMkdir args False
        result <- mkdir time "default" ["/"] cmd
        return result
      cmd:_ -> do
        liftIO $ processMode args0
        return (Nothing, [], ["command not found: "++cmd])
  case record_ of
    (record', warn, err) -> do
      liftIO $ mapM_ putStrLn err
      liftIO $ mapM_ putStrLn warn
      case record' of
        Nothing -> return ()
        Just record -> liftIO $ print record
  repl

data CommandLs = CommandLs {
  lsArgs :: [String]
}

ls :: UTCTime -> String -> [FilePath] -> CommandLs -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
ls time user cwd cmd = do
  x_ <- mapM lsone chain_l
  case concatEithersN x_ of
    Left msgs -> return (Left msgs)
    Right _ -> return (Right ())
  where
    args' = (\x -> if null x then ["."] else x) (lsArgs cmd)
    chain_l = map (pathStringToPathChain cwd) args'
    lsone :: [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
    lsone chain = do
      uuid_ <- pathChainToUuid chain
      case uuid_ of
        Left msgs -> return (Left msgs)
        Right Nothing -> do
          liftIO $ putStrLn "/"
          lschildren Nothing
          return (Right ())
        Right (Just uuid) -> do
          item__ <- getBy $ ItemUniqUuid uuid
          case item__ of
            Nothing ->
              return (Left ["couldn't load item from database: "++uuid])
            Just item_ -> do
              lsitem (entityVal item_)
    lsitem :: Item -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
    lsitem item =
      case itemType item of
        "folder" -> do
          liftIO $ putStrLn $ (itemToName item) ++ "/"
          lschildren (Just $ itemUuid item)
          return (Right ())
        "list" -> do
          liftIO $ putStrLn $ (itemToName item) ++ "/"
          lschildren (Just $ itemUuid item)
          return (Right ())
        "task" -> do
          liftIO $ putStrLn $ (itemToName item)
          lschildren (Just $ itemUuid item)
          return (Right ())
    lschildren :: Maybe String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
    lschildren parent = do
      items_ <- selectList [ItemParent ==. parent] []
      let items = map entityVal items_
      mapM_ lsitem items
      return (Right ())

itemToName :: Item -> String
itemToName item =
  fromMaybe (itemUuid item) (itemLabel item)

pathChainToUuid :: [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (Maybe String))
pathChainToUuid chain = parentToPathChainToUuid Nothing chain

parentToPathChainToUuid :: Maybe String -> [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (Maybe String))
parentToPathChainToUuid parent [] = return (Right parent)
parentToPathChainToUuid parent (name:rest) = do
  item_ <- nameToItem parent name
  case item_ of
    Left msgs -> return (Left msgs)
    Right item -> parentToPathChainToUuid (Just $ itemUuid item) rest


nameToItem :: Maybe String -> FilePath -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
nameToItem parent name = do
  item_ <- selectList [ItemLabel ==. (Just name), ItemParent ==. parent] [LimitTo 2]
  case item_ of
    [] -> return (Left ["No such file or directory"])
    p:[] -> return (Right $ entityVal p)
    _ -> return (Left ["conflict: multiple items at path"])

data CommandMkdir = CommandMkdir {
  mkdirArgs :: [String],
  mkdirParents :: Bool
}

pathStringToPathChain :: [FilePath] -> String -> [FilePath]
pathStringToPathChain cwd path_s = chain3 where
  -- Prepend cwd if relative path is given
  chain0 = case splitDirectories path_s of
    l@("/":rest) -> l
    rest -> cwd ++ rest
  -- Drop '/' prefixes
  chain1 = dropWhile (== "/") chain0
  -- Drop '.' infixes
  chain2 = filter (/= ".") chain1
  -- Drop x:".."
  dropDotDot :: [FilePath] -> [FilePath] -> [FilePath]
  dropDotDot [] acc = reverse acc
  dropDotDot (_:"..":rest) acc = dropDotDot rest acc
  dropDotDot (x:rest) acc = dropDotDot rest (x:acc)
  chain3 = dropDotDot chain2 []

mkdir :: UTCTime -> String -> [FilePath] -> CommandMkdir -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe CommandRecord, [String], [String])
--mkdir time user cwd cmd | trace "mkdir" False = undefined
mkdir time user cwd cmd = do
  if null args
    then return (Nothing, ["mkdir: missing operand", "Try 'mkdir --help' for more information."], [])
    else do
      result_ <- mapM mkone (mkdirArgs cmd)
      let ret = foldl (\(succ, warn, err) (succ', warn', err') -> (succ || succ', warn ++ warn', err ++ err')) (False, [], []) result_
      case ret of
        (True, warn, err) -> return (Just record, warn, err)
        (False, warn, err) -> return (Nothing, warn, err)
  where
    args = (mkdirArgs cmd)
    args' = args ++ (if mkdirParents cmd then ["--parents"] else [])
    record = CommandRecord 1 time (T.pack user) (T.pack "repl-mkdir") (map T.pack args')

    mkone :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Bool, [String], [String])
    --mkone path_s | trace ("mkone "++path_s) False = undefined
    mkone path_s =
      let
        chain = pathStringToPathChain cwd path_s
        --chains = tail $ inits chain
      in do
        -- Check whether the item already exists
        uuid_ <- pathChainToUuid chain
        case uuid_ of
          -- if the item already exists:
          Right _ -> do
            -- if --parents flag:
            if mkdirParents cmd
              then return (False, [], [])
              else return (False, [], ["mkdir: cannot create directory ‘"++path_s++"’: File exists"])
          -- if the item doesn't already exist:
          Left msgs -> do
            if mkdirParents cmd
              -- if all parents should be created
              then do
                uuid_ <- fn True chain Nothing
                case uuid_ of
                  Left msgs -> return (False, [], msgs)
                  Right uuid -> return (True, [], [])
              -- if all parents should already exist
              else do
                parent_ <- fn False (init chain) Nothing
                case parent_ of
                  Left msgs -> return (False, [], msgs)
                  Right parent -> do
                    new_ <- mksub parent (last chain)
                    case new_ of
                      Left msgs -> return (False, [], msgs)
                      Right new -> do
                        return (True, [], [])

    fn :: Bool -> [FilePath] -> Maybe String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (Maybe String))
    fn doMake l parent | trace ("fn "++(show l)) False = undefined
    fn _ [] parent = return (Right parent)
    fn doMake (name:rest) parent = do
      item_ <- selectList [ItemLabel ==. (Just name), ItemParent ==. parent] [LimitTo 2]
      case item_ of
        [] -> if doMake
          then do
            new_ <- mksub parent name
            case new_ of
              Left msgs -> return (Left msgs)
              Right new -> fn doMake rest (Just $ itemUuid new)
          else return (Left ["mkdir: cannot create directory ‘a/b/c’: No such file or directory"])
        p:[] -> fn doMake rest (Just $ itemUuid $ entityVal p)
        _ -> return (Left ["conflict: multiple items at path"])

    mksub :: Maybe String -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
    mksub parent name = do
      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
      let item = (itemEmpty uuid time "folder" name "open") { itemParent = parent, itemLabel = Just name }
      insert item
      return (Right item)


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
