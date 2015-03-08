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

module OnTopOfThings.Commands.Add
( mode_add
, modeInfo_add
--, optsProcess1_add -- for Import command
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (mplus)
import Data.Maybe
import Data.Monoid
import Debug.Trace
import System.Console.CmdArgs.Explicit
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
-- Database-related imports
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sqlite
-- Time-related imports
import Data.Time.Clock
import Data.Time.Format
import Data.Time.ISO8601
import System.Locale (defaultTimeLocale)

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env
import OnTopOfThings.Actions.Run
import OnTopOfThings.Actions.Utils
import OnTopOfThings.Commands.Utils

modeInfo_add :: ModeInfo
modeInfo_add = (mode_add, ModeRunDB optsProcess1_add optsProcess2_add optsRun_add)

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
    [ flagReq ["parent", "p"] (upd1 "parent") "ID" "reference to parent of this item"
    , flagReq ["closed"] (upd1 "closed") "TIME" "Time that this item was closed."
    , flagReq ["id"] (upd1 "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagReq ["label", "l"] (upd1 "label") "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (upd1 "stage") "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["status"] (upd1 "status") "STATUS" "open|closed|deleted. (default=open)"
    , flagReq ["tag", "t"] (updN "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["title"] (upd1 "title") "TITLE" "Title of the item."
    , flagReq ["type"] (upd1 "type") "TYPE" "list|task. (default=task)"
    , flagReq ["newfolder", "F"] (upd1 "newfolder") "FOLDER" "Place item in folder, and create the folder if necessary."
    , flagHelpSimple updHelp
    ]
  }

-- move the argument to the 'title' field
optsProcess1_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_add opts0 = do
  uuid <- liftIO $ U4.nextRandom >>= return . U.toString
  -- replace references with uuids where necessary,
  flags_l_ <- mapM refToUuid' (optionsFlags opts0)
  return $ getArgs uuid flags_l_
  where
    getArgs :: String -> [Validation (String, String)] -> Validation Options
    getArgs uuid flags_l_ = do
      flags' <- concatEithersN flags_l_
      let opts' = opts0 { optionsFlags = flags' }
      -- Get the title field
      let title1_ = M.lookup "title" (optionsMap opts') >>= \x -> x -- title set in flags
      let title2_ = listToMaybe (optionsArgs opts') -- title as first argument
      title <- maybeToEither ["A title must be supplied"] (title1_ `mplus` title2_)
      -- Add 'id' and 'title'
      let defaults = [("id", uuid), ("title", title)]
      let opts'' = foldl optionsSetDefault opts' defaults where
      return opts''

optsProcess2_add :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_add opts = return (Right opts') where
  defaults = [("type", "task"), ("status", "open"), ("stage", "new")]
  opts' = foldl optionsSetDefault opts defaults where

optsRun_add :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
optsRun_add record opts0 = do
  let time = Command.commandTime record
  opts1_ <- case M.lookup "newfolder" (optionsParams1 opts0) of
    Nothing -> return (Right opts0)
    Just folder -> do
      let env0 = Env time "default" ["/"]
      let actionMkdir = ActionMkdir [folder] Nothing True
      runAction env0 actionMkdir
      parent_ <- fullPathStringToUuid folder
      case parent_ of
        Left msgs -> return (Left msgs)
        Right Nothing -> return (Right opts0)
        Right (Just parent) -> do
          let opts1_ = upd1 "parent" parent opts0
          case opts1_ of
            Left msg -> return (Left [msg])
            Right opts1 -> return (Right opts1)
  case opts1_ of
    Left msgs -> return (Left msgs)
    Right opts1 -> do
      case OnTopOfThings.Commands.Utils.createItem time opts1 of
        Left msgs -> return (Left msgs)
        Right item -> do
          insert item
          mapM_ (saveProperty (itemUuid item)) (optionsMods opts1)
          return $ Right ()

--refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
refToUuid' ("parent", ref) = do
  uuid_ <- refToUuid ref
  return $ fmap (\uuid -> ("parent", uuid)) uuid_
refToUuid' x = return $ Right x
