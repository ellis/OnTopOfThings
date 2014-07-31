{-
Copyright (C) 2013,2014  Ellis Whitehead

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

module OnTopOfThings.Commands.Export
( modeInfo_export
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum)
import Data.Generics.Aliases (orElse)
import Data.List (inits, intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mempty)
import Data.Time.Clock
import Data.Time.LocalTime (utc)
import Data.Time.Format (parseTime)
import Data.Time.ISO8601 (formatISO8601, formatISO8601Millis)
import Database.Persist
import Database.Persist.Sql (insert, deleteWhere, toPersistValue)
import Database.Persist.Sqlite (SqlPersistT, runSqlite, rawSql)
import Database.Persist.Types
import Debug.Hood.Observe
import Debug.Trace
import System.Console.CmdArgs.Explicit
import System.Directory (getDirectoryContents)
import System.FilePath (joinPath, splitDirectories, takeExtension, takeFileName)
import System.IO
import System.Locale (defaultTimeLocale)
--import qualified System.FilePath.Find as FF
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import Args
import Command (CommandRecord(..))
import DatabaseTables
import Utils
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Json
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.Time
import OnTopOfThings.Data.Types

modeInfo_export :: ModeInfo
modeInfo_export = (mode_export, ModeRunIO optsRun_export)

mode_export = Mode
  { modeGroupModes = mempty
  , modeNames = ["export"]
  , modeValue = options_empty "export"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Export JSON snapshot of database"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg updArgs "ID"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["output", "o"] (upd "output") "FILE" "file to use for output"
    , flagHelpSimple updHelp
    ]
  }

optsValidate :: Options -> Validation ()
optsValidate opts = do
  --when (null $ optionsArgs opts) (Left ["You must supply a filename"])
  Right ()

optsRun_export :: Options -> IO (Validation ())
optsRun_export opts = do
  case optsValidate opts of
    Left msgs -> return (Left msgs)
    Right () -> do
      runSqlite "repl.db" $ do
        let filters = [ItemType ==. "task", ItemStatus !=. "deleted"]
        tasks' <- selectList filters []
        let tasks = map entityVal tasks'
        liftIO $ putStrLn "tasks:"
        liftIO $ mapM_ print tasks
        return (Right ())
