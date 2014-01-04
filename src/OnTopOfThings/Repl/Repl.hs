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

module OnTopOfThings.Repl.Repl
( mode_root
, modeInfo_
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
import System.FilePath.Posix (splitDirectories)
import System.Locale (defaultTimeLocale)

import Args
import Command
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Commands.Utils
import OnTopOfThings.Parsers.NumberList

modeInfo_ls = (mode_ls, ModeRunRO optsRun_ls)
modeInfo_mkdir = (mode_mkdir, ModeRunRW optsCannonize_mkdir optsRun_mkdir)

mode_ls = Mode
  { modeGroupModes = mempty
  , modeNames = ["ls"]
  , modeValue = options_empty "ls"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "List information about the FILEs (the current directory by default)."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "FILE"))
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

--optsRun_ls :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
--optsRun_ls opts = do

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

-- move the argument to the 'title' field
optsProcess1_mkdir :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsProcess1_mkdir opts0 = do
  if null (optionsArgs opts0)
    then return (Left ["mkdir: missing operand", "Try 'mkdir --help' for more information."])
    else return opts0

optsCannonize_mkdir :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Options)
optsCannonize_mkdir opts = return (Right opts)

optsRun_mkdir :: CommandRecord -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
optsRun_mkdir record opts = do
  mapM mkone (optionsArgs opts0)
  where
    mkone path = do
      splitDirectories 
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

data CommandLs = CommandLs {
  lsArgs :: [String]
}

ls :: UTCTime -> String -> [FilePath] -> CommandLs -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (CommandRecord))
ls time user cwd cmd = do
  mapM mkone (mkdirArgs cmd)

data CommandMkdir = CommandMkdir {
  mkdirArgs :: [String],
  mkdirParents :: Bool
}

mkdir :: UTCTime -> String -> [FilePath] -> CommandMkdir -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (CommandRecord))
mkdir time user cwd cmd = do
  mapM mkone (mkdirArgs cmd)
  where
    mkone path0 = do
      let path = case splitDirectories path0 of
        l@('/':rest) -> dropWhile (== '/') l
        rest -> dropWhile (== '/') (cwd ++ rest)
      parent_ <- fn path Nothing
      case parent_ of
        Left msgs -> return (Left msgs)
        Right parent -> do
          new_ <- mksub parent name
          case new_ of

    fn :: [FilePath] -> Maybe String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation (Maybe String))
    fn [] parent = return (Right parent)
    fn (name:rest) parent = do
      item_ <- selectList [ItemLabel ==. (Just name) &&. ItemParent ==. parent] [LimitTo 2]
      case item_ of
        [] -> if (mkdirParents cmd)
          then do
            new_ <- mksub parent name
            case new_ of
              Left msgs -> return (Left msgs)
              Right new -> fn rest (itemUuid new)
          else return (Left ["mkdir: cannot create directory ‘a/b/c’: No such file or directory"])
        p:[] -> fn rest (itemUuid $ entityVal p)
        _ -> return (Left ["conflict: multiple items at path"])

    mknew :: String -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
    mknew parent name = do
      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
      let item = (itemEmpty uuid time "folder" name "open") { itemParent = Some parent, itemLabel = Some name }
      insert item
      return (Right item)

getPathItemsR :: [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation [Item])
getPathItemsR path = do
  foldl fn path
  fn :: [Item] -> FilePath -> [Item]
  fn acc name = do

