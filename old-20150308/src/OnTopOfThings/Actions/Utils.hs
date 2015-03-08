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

module OnTopOfThings.Actions.Utils where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.ISO8601
import Database.Persist (insert)
import Database.Persist.Sqlite
import Debug.Trace
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories)
import System.IO
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.Yaml as Yaml

import Args
import DatabaseTables
import DatabaseUtils
import Utils
import OnTopOfThings.Parsers.NumberList
import OnTopOfThings.Actions.Action
import OnTopOfThings.Actions.Env
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Json
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase
import OnTopOfThings.Data.Types


itemToYamlLines :: Item -> BS.ByteString
itemToYamlLines item =
  -- Problem with using Yaml.encode is that the properties are not ordered
  Yaml.encode (ItemForJson item M.empty) -- TODO: load item properties
--  l' = concat
--    [ get "uuid" itemUuid
--    , get "type" itemType
--    , get "creator" itemCreator
--    , get "status" itemStatus
--    , getMaybe "name" itemName
--    , getMaybe "title" itemTitle
--    , getMaybe "content" itemContent
--    , getMaybe "stage" itemStage
--    ]
--  get :: String -> (Item -> String) -> [String]
--  get name fn = [name ++ ": " ++ (fn item)]
--  getMaybe :: String -> (Item -> Maybe String) -> [String]
--  getMaybe name fn = maybeToList $ fmap (\x -> name ++ ": " ++ x) (fn item)

itemToName :: Item -> String
itemToName item =
  fromMaybe (itemUuid item) (itemName item)

absPathChainToItems :: [FilePath] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation [Maybe Item])
absPathChainToItems chain = do
  root_ <- getroot
  case root_ of
    Left msgs -> return (Left msgs)
    Right root -> do
      -- REFACTOR: this is a very inefficient way to get the items
      items_ <- mapM (parentToPathChainToItem root) chain_l
      let items = map fn items_
      return (Right items)
  where
    -- Only keep part of chain after the root
    chain' = reverse $ takeWhile (/= "/") $ reverse chain
    chain_l = tail $ inits chain'
    -- Validation to Maybe
    fn :: Validation Item -> Maybe Item
    fn (Left _) = Nothing
    fn (Right item) = Just item

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
-- Ignore '.'
parentToPathChainToItem parent (".":rest) =
  parentToPathChainToItem parent rest
-- '..' to move up to parent folder
parentToPathChainToItem item ("..":rest) =
  case itemParent item of
    Nothing -> parentToPathChainToItem item rest
    Just parentUuid -> do
      parent_ <- uuidToItem parentUuid
      case parent_ of
        Left msgs -> return (Left msgs)
        Right parent -> parentToPathChainToItem parent rest
-- '/' is for the root
parentToPathChainToItem _ ("/":rest) = do
  item_ <- getroot
  case item_ of
    Left msgs -> return (Left msgs)
    Right item -> parentToPathChainToItem item rest
-- Look for subdirectory
parentToPathChainToItem parent (name:rest) = do
  item_ <- nameToItem (Just $ itemUuid parent) name
  case item_ of
    Left msgs -> do
      entity' <- getBy $ ItemUniqUuid name
      case entity' of
        Nothing -> return (Left msgs)
        Just entity -> parentToPathChainToItem (entityVal entity) rest
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
  item_ <- selectList [ItemName ==. (Just name), ItemParent ==. parent] [LimitTo 2]
  case item_ of
    [] -> return (Left ["No such file or directory"])
    p:[] -> return (Right $ entityVal p)
    _ -> return (Left ["conflict: multiple items at path"])

pathStringToAbsPathChain :: [FilePath] -> String -> [FilePath]
pathStringToAbsPathChain cwd path_s = chain4 where
  -- Prepend cwd if relative path is given
  chain0 = case splitDirectories path_s of
    l@("/":rest) -> l
    rest -> cwd ++ rest
  -- Drop everything before last '/'
  chain1 = reverse $ takeWhile (/= "/") $ reverse chain0
  -- Drop '.' infixes
  chain2 = filter (/= ".") chain1
  -- Drop x:".."
  dropDotDot :: [FilePath] -> [FilePath] -> [FilePath]
  dropDotDot [] acc = reverse acc
  dropDotDot ("..":rest) acc = dropDotDot rest acc
  dropDotDot (_:"..":rest) acc = dropDotDot rest acc
  dropDotDot (x:rest) acc = dropDotDot rest (x:acc)
  chain3 = dropDotDot chain2 []
  -- Prefix root '/'
  chain4 = "/":chain3

pathStringToItem :: [FilePath] -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
pathStringToItem cwd path_s = do
  let chain = pathStringToAbsPathChain cwd path_s
  absPathChainToItem chain

-- REFACTOR: remove this function
pathStringToPathChain :: [FilePath] -> String -> [FilePath]
pathStringToPathChain cwd path_s = chain3 where
  -- Prepend cwd if relative path is given
  chain0 = case splitDirectories path_s of
    l@("/":rest) -> l
    rest -> cwd ++ rest
  -- Drop everything before last '/'
  chain1 = reverse $ takeWhile (/= "/") $ reverse chain0
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
      let itemRoot = (itemEmpty uuidRoot time "system" "folder" "open") { itemName = Just "/" }
      insert itemRoot
      return (Right itemRoot)

data PathInfo = PathInfo
  { pathInfoString :: String
  , pathInfoChainRel :: [FilePath]
  , pathInfoChainAbs :: [FilePath]
  , pathInfoItems :: [Maybe Item]
  } deriving (Show)

pathStringToPathInfo :: Env -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation PathInfo)
pathStringToPathInfo env path_s = do
  -- Split path string into a path chain
  let chainRel = splitDirectories path_s
  let chainAbs = pathStringToAbsPathChain (envCwdChain env) path_s
  -- Get the root item
  root_ <- getroot
  case root_ of
    Left msgs -> return (Left ["couldn't find root item"])
    Right root -> do
      items_ <- fn (tail chainAbs) (Just root)
      let items = (Just root) : items_
      return (Right $ PathInfo path_s chainRel chainAbs items)
  where
    -- From a filepath, get either the first path chain that doesn't exist or the item and its path chain
    fn :: [FilePath] -> Maybe Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ([Maybe Item])
    fn [] _ = return []
    fn (_:rest) Nothing = return $ Nothing : map (const Nothing) rest
    fn (name:rest) (Just parent) = do
      item_ <- parentToPathChainToItem parent [name]
      case item_ of
        Left msgs -> return $ Nothing : map (const Nothing) rest
        Right item -> do
          rest_ <- fn rest (Just item)
          return $ (Just item) : rest_

lookupItem :: Env -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
lookupItem env ref = do
  -- Try ID
  item1_ <- getBy $ ItemUniqUuid ref
  case item1_ of
    Just item -> return (Right (entityVal item))
    Nothing -> do
      -- Try path
      let path = joinPath ((envCwdChain env) ++ [ref])
      let chain = splitDirectories path
      item2_ <- absPathChainToItem chain
      case item2_ of
        Right item -> return (Right item)
        Left msgs -> do
          -- Try index
          let n' = readMaybe ref :: Maybe Int
          case n' of
            Nothing -> return (Left (("couldn't find item: "++ref) : msgs))
            Just n -> do
              l <- selectList [ItemIndex ==. (Just n)] [LimitTo 2]
              case l of
                [] -> return (Left (("couldn't find item: "++ref) : msgs))
                entity:[] -> return (Right (entityVal entity))
                _:_:[] -> return (Left ["Internal error.  Multiple items with given index: "++(show n)])

itemToAbsPathChain :: Item -> SqlPersistT (NoLoggingT (ResourceT IO)) [FilePath]
itemToAbsPathChain item = do
  parentChain <- case (itemParent item) of
    Nothing -> return []
    Just uuid -> do
      parent_ <- getBy $ ItemUniqUuid uuid
      case parent_ of
        Nothing -> return []
        Just parent -> do
          parentChain <- itemToAbsPathChain (entityVal parent)
          return parentChain
  return $ parentChain ++ [fromMaybe (itemUuid item) (itemName item)]

uuidToItem :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
uuidToItem uuid = do
  entities <- selectList [ItemUuid ==. uuid] [LimitTo 2]
  case entities of
    [] -> return (Left ["couldn't find item UUID: "++uuid])
    entity:[] -> return (Right (entityVal entity))
    _:_:[] -> return (Left ["Internal error.  Multiple items with given UUID: "++uuid])
