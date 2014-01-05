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

module OnTopOfThings.Actions.Action where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Environment
import System.FilePath.Posix (splitDirectories)
import System.IO
import Database.Persist (insert)
import Database.Persist.Sqlite
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Args
import Utils
import OnTopOfThings.Actions.Env

data ActionLs = ActionLs
    { lsArgs :: [String]
    } deriving (Show)

data ActionMkdir = ActionMkdir
    { mkdirArgs :: [String]
    , mkdirParents :: Bool
    } deriving (Show)

data ActionNewItem = ActionNewItem
    { newItemType :: String
    , newItemParentRef :: Maybe String
    , newItemName :: Maybe String
    , newItemTitle :: Maybe String
    , newItemContent :: Maybe String
    } deriving (Show)

data ActionResult = ActionResult
  { actionResultChange :: Bool
  , actionResultRollback :: Bool
  , actionResultWarnings :: [String]
  , actionResultErrors :: [String]
  } deriving (Show)

instance Monoid ActionResult where
  mempty = ActionResult False False [] []
  mappend (ActionResult c r w e) (ActionResult c' r' w' e') = ActionResult (c || c') (r || r') (w ++ w') (e ++ e')

type SqlActionResult = SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)

class Action a where
  runAction :: Env -> a -> SqlPersistT (NoLoggingT (ResourceT IO)) (Env, ActionResult)
  actionFromOptions :: (Action a) => Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation a)

