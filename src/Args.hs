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
, ModeRun(..)
, ModeInfo(..)
, options_empty
, optionsSetDefault
, reform
, upd
, updArgs
, updHelp
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

import Command (CommandRecord)
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

data ModeRun
  = ModeRunDB
    { modeRunProcess1 :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
    , modeRunProcess1 :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
    , modeRunDB :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
    }
  | ModeRunIO
    { modeRunIO :: Options -> IO (Validation ()) }

type ModeInfo =
  ( Mode Options, ModeRun )

options_empty :: String -> Options
options_empty name = Options name [] [] False [] M.empty

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

-- Function to add a default flag value if no value was already set
-- TODO: this function won't work correctly if a value isn't in 'optionsMap'.
optionsSetDefault :: Options -> (String, String) -> Options
optionsSetDefault acc (name, value) = case M.lookup name (optionsMap acc) of
  Nothing ->
    case upd name value acc of
      Left msg -> acc
      Right acc' -> acc'
  Just x -> acc

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
