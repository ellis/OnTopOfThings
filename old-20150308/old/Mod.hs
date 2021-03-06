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

module OnTopOfThings.Commands.Mod
( mode_mod
, modeInfo_mod
--, optsProcess1_mod -- for Import command
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
import OnTopOfThings.Commands.Utils
import OnTopOfThings.Parsers.NumberList

modeInfo_mod :: ModeInfo
modeInfo_mod = (mode_mod, ModeRunDB optsProcess1_mod optsProcess2_mod optsRun_mod)

mode_mod = Mode
  { modeGroupModes = mempty
  , modeNames = ["mod"]
  , modeValue = options_empty "mod"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Modify an existing item"
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "ID"))
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (upd1 "parent") "ID" "reference to parent of this item"
    , flagReq ["closed"] (upd1 "closed") "TIME" "Time that this item was closed."
    , flagReq ["id"] (updN "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagReq ["label", "l"] (upd1 "label") "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (upd1 "stage") "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["status"] (upd1 "status") "STATUS" "open|closed|deleted. (default=open)"
    , flagReq ["tag", "t"] (updN "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["title"] (upd1 "title") "TITLE" "Title of the item."
    , flagReq ["type"] (upd1 "type") "TYPE" "list|task. (default=task)"
    , flagHelpSimple updHelp
    ]
  }

-- move the argument to the 'title' field
optsProcess1_mod :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_mod opts0 = do
  opts1_ <- processRefArgsAndFlags opts0 "id"
  case opts1_ of
    Left msgs -> return (Left msgs)
    Right opts1 -> do
      opts2_ <- processRefFlags opts1 "parent"
      return opts2_

optsProcess2_mod :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess2_mod opts = return (Right opts)

optsRun_mod :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
--optsRun_mod record opts | trace ("optsRun_mod: "++(show record)) False = undefined
optsRun_mod record opts = do
  let time = Command.commandTime record
  let m = optionsMap opts
  case M.lookup "id" (optionsParamsN opts) of
    Just uuids -> do
      x_ <- mapM (fn time m) uuids
      return $ concatEithersN x_ >>= const (Right ())
    _ -> return (Left ["You must specify ID(s) for item(s) to modify"])
  where
    fn time m uuid = do
      entity_ <- getBy (ItemUniqUuid uuid)
      case entity_ of
        Nothing -> return (Left ["Could not find item with given id"])
        Just entity -> do
          let item0 = entityVal entity
          case updateItem time m item0 of
            Left msgs -> return (Left msgs)
            Right item -> do
              replace (entityKey entity) item
              mapM_ (saveProperty uuid) (optionsMods opts)
              return (Right ())

--refToUuid :: (String, String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String))
refToUuid' ("id", ref) = do
  uuid_ <- refToUuid ref
  return $ fmap (\uuid -> ("id", uuid)) uuid_
refToUuid' x = return $ Right x
