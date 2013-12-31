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

module Args
( Options(..)
, Mod(..)
--, mode_empty
, mode_add
, mode_close
, mode_rebuild
, mode_root
, reform
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Maybe
import Data.Monoid
import Database.Persist.Sqlite
import System.Console.CmdArgs.Explicit
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import DatabaseUtils
import Utils

data Options = Options
  { optionsCmd :: String
  , optionsArgs :: [String]
  , optionsFlags :: [(String, String)]
  , optionsHelp :: Bool
  , optionsMods :: [Mod]
  , optionsMap :: M.Map String (Maybe String)
  }
  deriving (Show)

data Mod
  = ModNull String
  | ModEqual String String
  | ModAdd String String
  | ModUnset String
  | ModRemove String String
  deriving Show

options_empty :: String -> Options
options_empty name = Options name [] [] False [] M.empty

modeInfo
:: M.Map
  String
  ( Mode
  , Maybe (Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options))
  , Maybe (Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options))
  , Maybe (CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options))
  )
modeInfo = M.fromList
  [ ("add", mode_add, Just optsProcess1_add, Just optsProcess2_add, Just optsRun_add)
  , ("close", mode_close, Nothing)
  , ("rebuild", mode_rebuild, Nothing, Just optsRun_rebuild)
  ]

mode_add = Mode
  { modeGroupModes = mempty
  , modeNames = ["add"]
  , modeValue = options_empty "add"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Add a new task"
  , modeHelpSuffix = ["Add a new task and be a dude"]
  , modeArgs = ([flagArg updArgs "TITLE"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (upd "parent") "ID" "reference to parent of this item"
    , flagReq ["label", "l"] (upd "label") "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (upd "stage") "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["tag", "t"] (upd "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["type"] (upd "type") "TYPE" "list|task. (default=task)"
    , flagHelpSimple updHelp
    ]
  }

optsProcess1_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_add args = do
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  flags' <- mapM refToUuid (optionsFlags args)
  return $ getArgs uuid flags'
  where
    getArgs :: String -> [Either String (String, String)] -> Validation Options
    getArgs uuid flags' =
      case concatEithers1 flags' of
        Left msgs -> Left msgs
        Right flags'' -> Right $ args { optionsFlags = flags''' } where
          flags''' :: [(String, String)]
          flags''' = ("id", uuid) : flags''

optsProcess2_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_add opts = return (Right opts') where
  defaults = [("status", "open"), ("stage", "new")]
  map' = foldl setDefault (optionsMap opts) defaults where
  opts' = opts { optionsMap = map' }
  -- Function to add a default value if no value was already set
  setDefault :: M.Map String (Maybe String) -> (String, String) -> M.Map String (Maybe String)
  setDefault acc (name, value) = case M.lookup name acc of
    Nothing -> M.insert name (Just value) acc
    Just x -> acc

optsRun_add :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsRun_add record opts = do
  case createItem (commandTime record) opts of
    Left msgs -> return (Left msgs)
    Right item -> do
      insert item
      mapM_ (saveProperty uuid) args
      return $ Right ()

mode_close = mode "close" (options_empty "") "close an item" (flagArg updArgs "REF")
  [-- flagHelpSimple (("help", ""):)
  ]

mode_rebuild = Mode
  { modeGroupModes = mempty
  , modeNames = ["rebuild"]
  , modeValue = options_empty "rebuild"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Rebuild the Sqlite database."
  , modeHelpSuffix = []
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup
    [ flagHelpSimple updHelp
    ]
  }

optsRun_rebuild _ opts = do
  -- 1) load the command records from files
  x <- loadCommandRecords
  case x of
    Right records -> do
      -- TODO: only show this if --verbose
      mapM_ (putStrLn . show) records
      runSqlite "otot.db" $ do
        DB.databaseInit
        -- 2) convert the command records to and SQL 'command' table
        -- 3) process the 'command' table, producing the 'property' table
        DB.databaseAddRecords records
        DB.databaseUpdateIndexes

mode_root = modes "otot" (options_empty "") "OnTopOfThings for managing lists and tasks"
  [mode_add, mode_close, mode_rebuild]

updArgs value opts = Right opts' where
  args' = optionsArgs opts ++ [value]
  opts' = opts { optionsArgs = args' }

upd :: String -> String -> Options -> Either String Options
upd name value opts = Right opts' where
  flags' = optionsFlags opts ++ [(name, value)]
  mods' = optionsMods opts ++ (catMaybes [if null value then Nothing else Just (ModEqual name value)])
  map' = M.insert name (Just value) (optionsMap opts)
  opts' = opts { optionsFlags = flags', optionsMods = mods', optionsMap = map' }

updHelp opts = opts { optionsHelp = True }

--refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
refToUuid ("parent", ref) = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left "Couldn't find parent ref")
    Just uuid -> return (Right $ ("parent", uuid))
refToUuid x = return $ Right x

reform :: Options -> [String]
reform args = l where
  l1 = map doFlag (optionsFlags args)
  l = l1 ++ (optionsArgs args)
  doFlag (name, value) = if null value then ("--"++name) else ("--"++name++"="++value)

---myModes :: Mode (CmdArgs MyOptions)
---myModes = cmdArgsMode $ modes [options_add, options_close]
---    &= verbosityArgs [explicit, name "Verbose", name "V"] []
---    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
---    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
---    &= help _PROGRAM_ABOUT
---    &= helpArg [explicit, name "help", name "h"]
---    &= program _PROGRAM_NAME
---
---_PROGRAM_NAME = "otot"
---_PROGRAM_VERSION = "0.0-a"
---_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
---_PROGRAM_ABOUT = "a sample CmdArgs program for you tinkering pleasure"
---_COPYRIGHT = "(C) Ellis Whitehead 2013"
