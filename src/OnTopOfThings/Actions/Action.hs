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
import Command (CommandRecord)
import Utils
import OnTopOfThings.Data.Card (CardItem)
import OnTopOfThings.Actions.Env

data ActionLs = ActionLs
    { lsArgs :: [String]
    , lsRecursive :: Bool
    } deriving (Show)

data ActionMkdir = ActionMkdir
    { mkdirArgs :: [String]
    , mkdirUuid :: Maybe String
    , mkdirParents :: Bool
    } deriving (Show)

data ActionNewTask = ActionNewTask
    { newTaskHelp :: Bool
    , newTaskUuid :: Maybe String
    , newTaskParentRef :: Maybe String
    , newTaskName :: Maybe String
    , newTaskTitle :: Maybe String
    , newTaskContent :: Maybe String
    , newTaskStatus :: Maybe String
    , newTaskStage :: Maybe String
    , newTaskTags :: [String]
    } deriving (Show)

data ActionResult = ActionResult
  { actionResultCards :: [CardItem]
  , actionResultRollback :: Bool
  , actionResultWarnings :: [String]
  , actionResultErrors :: [String]
  } deriving (Show)

instance Monoid ActionResult where
  mempty = ActionResult [] False [] []
  mappend (ActionResult c r w e) (ActionResult c' r' w' e') = ActionResult (c ++ c') (r || r') (w ++ w') (e ++ e')

type SqlActionResult = SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)

class Action a where
  runAction :: Env -> a -> SqlPersistT (NoLoggingT (ResourceT IO)) (Env, ActionResult)
  actionFromOptions :: (Action a) => Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation a)
  actionToRecordArgs :: (Action a) => a -> Maybe [String]

