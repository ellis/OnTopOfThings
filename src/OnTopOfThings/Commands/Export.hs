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
import Data.Aeson.Types (Pair, Parser)
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

data ItemExport = ItemExport Item PropertyMap

instance ToJSON ItemExport where
  toJSON (ItemExport item properties) = object l where
    l = catMaybes
      [ get "id" itemUuid
      , getDate "created" itemCreated
      , get "creator" itemCreator
      , get "type" itemType
      --, get "status" itemStatus
      , if (itemStatus item) == "deleted" then (Just ("deleted" .= (T.pack "true"))) else Nothing
      , getMaybe "parent" itemParent
      , getMaybe "name" itemName
      , getMaybe "title" itemTitle
      , getMaybe "content" itemContent
      , getMaybe "horizon" itemStage
      , getMaybe "closed" itemClosed
      , getMaybe "start" itemStart
      , getMaybe "end" itemEnd
      , getMaybe "due" itemDue
      , getMaybe "defer" itemDefer
      , getMaybeInt "estimate" itemEstimate
      , M.lookup "tag" properties >>= \tags -> Just ("tag" .= Array (V.fromList (map (String . T.pack) tags)))
      ]
    get :: T.Text -> (Item -> String) -> Maybe Pair
    get name fn = Just (name .= (T.pack $ fn item))

    getDate :: T.Text -> (Item -> UTCTime) -> Maybe Pair
    getDate name fn = Just (name .= (T.pack $ formatISO8601 (fn item)))

    getMaybe :: T.Text -> (Item -> Maybe String) -> Maybe Pair
    getMaybe name fn = fmap (\x -> name .= String (T.pack x)) (fn item)

    getMaybeInt :: T.Text -> (Item -> Maybe Int) -> Maybe Pair
    getMaybeInt name fn = fmap (\x -> name .= (T.pack $ show x)) (fn item)

    getMaybeDate :: T.Text -> (Item -> Maybe UTCTime) -> Maybe Pair
    getMaybeDate name fn = fmap (\x -> name .= (T.pack $ formatISO8601 x)) (fn item)

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
        let itemForJson_l = map (\item -> ItemExport item mempty) tasks
        liftIO $ putStrLn "tasks:"
        --liftIO $ mapM_ (\item -> print (toJSON (ItemForJson item mempty))) tasks
        h <- liftIO $ return stdout
        liftIO $ BL.hPutStr h (encode itemForJson_l)
        return (Right ())
