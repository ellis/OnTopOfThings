{-
Copyright (C) 2014  Ellis Whitehead

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

module OnTopOfThings.Actions.Run where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (intercalate, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (splitDirectories)
import System.IO
import Database.Persist (insert)
import Database.Persist.Sqlite
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Parsers.NumberList
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env


instance Action ActionLs where
  runAction env cmd = ls env cmd >>= \x -> return (env, x)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionLs)
  actionFromOptions opts = let
      args = optionsArgs opts
      isRecursive = Set.member "recursive" (optionsParams0 opts)
    in do
      return (Right (ActionLs args isRecursive))

  actionToRecordArgs action = Nothing

instance Action ActionMkdir where
  runAction env cmd = mkdir env cmd >>= \x -> return (env, x)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionMkdir)
  actionFromOptions opts = do
    return (Right (ActionMkdir (optionsArgs opts) (Set.member "parents" (optionsParams0 opts))))
  actionToRecordArgs action = Just $ flags ++ args where
    args = mkdirArgs action
    flags = catMaybes $ [if mkdirParents action then Just "--parents" else Nothing]

mode_ls = Mode
  { modeGroupModes = mempty
  , modeNames = ["ls"]
  , modeValue = options_empty "ls"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "List information about the FILEs (in the current directory by default)."
  , modeHelpSuffix = ["Add a new task and be a dude"]
  , modeArgs = ([], Just (flagArg updArgs "FILE"))
  , modeGroupFlags = toGroup
    [ flagNone ["recursive", "R"] (upd0 "recursive") "list subdirectories recursively"
    , flagNone ["help"] updHelp "display this help and exit"
    ]
  }

mode_mkdir = Mode
  { modeGroupModes = mempty
  , modeNames = ["mkdir"]
  , modeValue = options_empty "mkdir"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Create the DIRECTORY(ies), if they do not already exist."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "DIRECTORY"))
  , modeGroupFlags = toGroup
    [ flagNone ["parents", "p"] (upd0 "parents") "no error if existing, make parent directories as needed"
    , flagHelpSimple updHelp
    ]
  }

ls :: Env -> ActionLs -> SqlActionResult
ls env action | trace ("ls: "++(show action)) False = undefined
ls (Env time user cwd) (ActionLs args0 isRecursive) = do
  itemTop_ <- mapM absPathChainToItem chain_l
  let (goodR, badR) = foldl partitionArgs ([], []) (zip args' itemTop_)
  let bad = reverse badR
  liftIO $ mapM_ putStrLn bad
  let good = reverse goodR
  let (folders, tasks) = partition partitionTypes good
  let items' = map snd tasks
  liftIO $ lsitems items'
  x_ <- mapM lsfolder folders
  return (mempty :: ActionResult)

  return (mconcat x_)
  where
    args' = (\x -> if null x then ["."] else x) args0
    chain_l = map (pathStringToPathChain cwd) args'

    partitionArgs :: ([(String, Item)], [String]) -> (String, Validation Item) -> ([(String, Item)], [String])
    partitionArgs (good, bad) (arg, Right item) = ((arg, item):good, bad)
    partitionArgs (good, bad) (arg, Left msgs) = (good, (map (\s -> "ls: cannot access "++arg++": "++s) msgs) ++ bad)

    -- Return true if this if passed a folder or list item (or Nothing item, for the root folder)
    partitionTypes :: (String, Item) -> Bool
    partitionTypes (_, item) = case itemType item of
      "folder" -> True -- TODO: but False if -d
      "list" -> True
      _ -> False

    lsitems :: [Item] -> IO ()
    lsitems items = do
      mapM_ (\l -> (putStrLn . intercalate " ") l) ll
      where
        ll = map strings items
        strings item = case itemType item of
          "folder" -> [(itemToName item) ++ "/"]
          "list" -> [(itemToName item) ++ "/"]
          _ -> [(itemToName item)]

    doShowFolderName = isRecursive || length args' > 1

    lsfolder :: (String, Item) -> SqlActionResult
    lsfolder (arg, item) = do
      let name = itemToName item
      let uuid = itemUuid item
      -- TODO: I probably want to use arg here
      when doShowFolderName (liftIO $ putStrLn (name++":"))
      items_ <- selectList [ItemParent ==. Just uuid] []
      let items = map entityVal items_
      liftIO $ lsitems items
      return mempty

itemToName :: Item -> String
itemToName item =
  fromMaybe (itemUuid item) (itemLabel item)

absPathChainToItem :: [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
absPathChainToItem chain = do
  root_ <- getroot
  case root_ of
    Left msgs -> return (Left msgs)
    Right root -> parentToPathChainToItem root chain' where
      -- Only keep part of chain after the root
      chain' = reverse $ takeWhile (/= "/") $ reverse chain

parentToPathChainToItem :: Item -> [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
parentToPathChainToItem parent [] = return (Right parent)
parentToPathChainToItem parent (name:rest) = do
  item_ <- nameToItem (Just $ itemUuid parent) name
  case item_ of
    Left msgs -> return (Left msgs)
    Right item -> parentToPathChainToItem item rest

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
  -- Prefix root '/'
  chain4 = "/":chain3

fullPathStringToUuid :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (Maybe String))
fullPathStringToUuid path_s = pathChainToUuid chain where
  chain = pathStringToPathChain [] path_s

uuidRoot = "00000000-0000-0000-0000-000000000000"

getroot :: SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
getroot = do
  root_ <- getBy $ ItemUniqUuid uuidRoot
  case root_ of
    Just root' -> return (Right (entityVal root'))
    Nothing -> do
      time <- liftIO $ getCurrentTime
      let itemRoot = (itemEmpty uuidRoot time "folder" "/" "open") { itemLabel = Just "/" }
      insert itemRoot
      return (Right itemRoot)

mkdir :: Env -> ActionMkdir -> SqlActionResult
mkdir env cmd | trace "mkdir" False = undefined
mkdir (Env time user cwd) cmd = do
  if null args
    then return (ActionResult False False ["mkdir: missing operand", "Try 'mkdir --help' for more information."] [])
    else do
      root_ <- getroot
      case root_ of
        Left msgs -> return (ActionResult False False [] msgs)
        Right root -> do
          result_ <- mapM (mkone root) (mkdirArgs cmd)
          return $ mconcat result_
  where
    args = (mkdirArgs cmd)
    args' = args ++ (if mkdirParents cmd then ["--parents"] else [])
    --record = CommandRecord 1 time (T.pack user) (T.pack "repl-mkdir") (map T.pack args')

    mkone :: Item -> String -> SqlActionResult
    --mkone path_s | trace ("mkone "++path_s) False = undefined
    mkone root path_s =
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
              then return mempty
              else return (ActionResult False False [] ["mkdir: cannot create directory ‘"++path_s++"’: File exists"])
          -- if the item doesn't already exist:
          Left msgs -> do
            if mkdirParents cmd
              -- if all parents should be created
              then do
                uuid_ <- fn True chain root
                case uuid_ of
                  Left msgs -> return (ActionResult False False [] msgs)
                  Right uuid -> return (ActionResult True False [] [])
              -- if all parents should already exist
              else do
                parent_ <- fn False (init chain) root
                case parent_ of
                  Left msgs -> return (ActionResult False False [] msgs)
                  Right parent -> do
                    new_ <- mksub parent (last chain)
                    case new_ of
                      Left msgs -> return (ActionResult False False [] msgs)
                      Right new -> do
                        return (ActionResult True False [] [])

    fn :: Bool -> [FilePath] -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
    fn doMake l parent | trace ("fn "++(show l)) False = undefined
    fn _ [] parent = return (Right parent)
    fn doMake (name:rest) parent = do
      item_ <- selectList [ItemLabel ==. (Just name), ItemParent ==. (Just (itemUuid parent))] [LimitTo 2]
      case item_ of
        [] -> if doMake
          then do
            new_ <- mksub parent name
            case new_ of
              Left msgs -> return (Left msgs)
              Right new -> fn doMake rest new
          else return (Left ["mkdir: cannot create directory ‘a/b/c’: No such file or directory"])
        p:[] -> fn doMake rest (entityVal p)
        _ -> return (Left ["conflict: multiple items at path"])

    mksub :: Item -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
    mksub parent name = do
      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
      let item = (itemEmpty uuid time "folder" name "open") { itemParent = (Just $ itemUuid parent), itemLabel = Just name }
      insert item
      return (Right item)
