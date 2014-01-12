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

module OnTopOfThings.Actions.Run where

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
import System.Console.ANSI
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath.Posix (joinPath, splitDirectories)
import System.IO
import Database.Persist (insert)
import Database.Persist.Sqlite
import Debug.Trace
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
import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase


instance Action ActionCat where
  runAction env action = cat env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionLs)
  actionFromOptions opts = let
      args = optionsArgs opts
    in do
      return (Right (ActionCat args))
  actionToRecordArgs action = Nothing

instance Action ActionLs where
  runAction env action = ls env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionLs)
  actionFromOptions opts = let
      args = optionsArgs opts
      isRecursive = Set.member "recursive" (optionsParams0 opts)
    in do
      return (Right (ActionLs args isRecursive))

  actionToRecordArgs action = Nothing

instance Action ActionMkdir where
  runAction env action = mkdir env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionMkdir)
  actionFromOptions opts = do
    return (Right (ActionMkdir (optionsArgs opts) (M.lookup "id" (optionsParams1 opts)) (Set.member "parents" (optionsParams0 opts))))
  actionToRecordArgs action = Just $ flags ++ args where
    args = mkdirArgs action
    flags = catMaybes
      [ mkdirUuid action >>= \x -> Just ("--id="++x)
      , if mkdirParents action then Just "--parents" else Nothing
      ]

instance Action ActionNewTask where
  runAction env action = newtask env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionMkdir)
  actionFromOptions opts = do
    let action_ = createAction
    return action_
    where
      createAction :: Validation ActionNewTask
      createAction = do
        id <- getMaybe "id"
        let title = unwords $ optionsArgs opts
        status <- get "status"
        parent <- getMaybe "parent"
        stage <- getMaybe "stage"
        label <- getMaybe "label"
        -- index
        closed <- getMaybeDate "closed"
        start <- getMaybeDate "start"
        end <- getMaybeDate "end"
        due <- getMaybeDate "due"
        review <- getMaybeDate "review"
        return $ ActionNewTask
          { newTaskHelp = False
          , newTaskUuid = id
          , newTaskParentRef = parent
          , newTaskName = label
          , newTaskTitle = Just title
          , newTaskContent = Nothing
          , newTaskStatus = Just status
          , newTaskStage = stage
          , newTaskTags = []
          }
      map = optionsMap opts
      get name = case M.lookup name map of
        Just (Just x) -> Right x
        _ -> Left ["missing value for `" ++ name ++ "`"]
      getMaybe name = case M.lookup name map of
        Just (Just s) -> Right (Just s)
        _ -> Right Nothing
      getMaybeDate :: String -> Validation (Maybe UTCTime)
      getMaybeDate name = case M.lookup name map of
        Just (Just s) ->
          (parseISO8601 s) `maybeToValidation` ["Could not parse time: " ++ s] >>= \time -> Right (Just time)
        _ -> Right Nothing

  actionToRecordArgs action = Just $ args ++ flags where
    args = maybeToList (newTaskTitle action)
    flags = catMaybes
      [ newTaskUuid action >>= \x -> Just ("--id="++x)
      , newTaskParentRef action >>= \x -> Just ("--parent="++x)
      , newTaskName action >>= \x -> Just ("--name="++x)
      ]

mode_cat = Mode
  { modeGroupModes = mempty
  , modeNames = ["cat"]
  , modeValue = options_empty "cat"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Concatenate FILE(s) to standard output."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "FILE"))
  , modeGroupFlags = toGroup
    [ flagNone ["help"] updHelp "display this help and exit"
    ]
  }


mode_ls = Mode
  { modeGroupModes = mempty
  , modeNames = ["ls"]
  , modeValue = options_empty "ls"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "List information about the FILEs (in the current directory by default)."
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "FILE"))
  , modeGroupFlags = toGroup
    [ flagNone ["recursive", "R"] (upd0 "recursive") "list subdirectories recursively"
    , flagNone ["help"] updHelp "display this help and exit"
    ]
  }

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

mode_newtask = Mode
  { modeGroupModes = mempty
  , modeNames = ["newtask"]
  , modeValue = ActionNewTask False Nothing Nothing Nothing Nothing Nothing Nothing Nothing []
  , modeCheck = Right
  , modeReform = const Nothing
  , modeExpandAt = True
  , modeHelp = "Add a new task"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg (\v a -> Right (a { newTaskTitle = Just v })) "TITLE"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (\v a -> Right (a { newTaskParentRef = Just v })) "ID" "reference to parent of this item"
    --, flagReq ["closed"] (upd1 "closed") "TIME" "Time that this item was closed."
    --, flagReq ["id"] (upd1 "id") "ID" "A unique ID for this item. (NOT FOR NORMAL USE!)"
    , flagReq ["label", "l"] (\v a -> Right (a { newTaskName = Just v })) "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (\v a -> Right (a { newTaskStage = Just v })) "STAGE" "new|incubator|today. (default=new)"
    , flagReq ["status"] (\v a -> Right (a { newTaskStatus = Just v })) "STATUS" "open|closed|deleted. (default=open)"
    --, flagReq ["tag", "t"] (updN "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    --, flagReq ["title"] (upd1 "title") "TITLE" "Title of the item."
    --, flagReq ["type"] (upd1 "type") "TYPE" "list|task. (default=task)"
    --, flagReq ["newfolder", "F"] (upd1 "newfolder") "FOLDER" "Place item in folder, and create the folder if necessary."
    , flagHelpSimple (\a -> a { newTaskHelp = True })
    ]
  }

cat :: Env -> ActionCat -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
cat env action | trace ("cat: "++(show action)) False = undefined
cat (Env time user cwd) action@(ActionCat args0) = do
  results_ <- mapM catone args0
  return (mconcat results_)
  where
    catone :: String -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
    catone path_s = do
      entity' <- getBy $ ItemUniqUuid path_s
      case entity' of
        Just entity -> catitem (entityVal entity)
        Nothing -> do
          let chain = pathStringToAbsPathChain cwd path_s
          item_ <- absPathChainToItem chain
          case item_ of
            Left msgs -> return (ActionResult [] False [] msgs)
            Right item -> catitem item
    catitem :: Item -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
    catitem item = do
      let lines = itemToYamlLines item
      --liftIO $ mapM_ putStrLn lines
      liftIO $ BS.putStr lines
      return mempty

itemToYamlLines :: Item -> BS.ByteString
itemToYamlLines item =
  -- Problem with using Yaml.encode is that the properties are not ordered
  Yaml.encode (ItemForJson item)
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

ls :: Env -> ActionLs -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
ls env action | trace ("ls: "++(show action)) False = undefined
ls (Env time user cwd) action@(ActionLs args0 isRecursive) = do
  itemTop_ <- mapM absPathChainToItem chain_l
  let (goodR, badR) = foldl partitionArgs ([], []) (zip args' itemTop_)
  let bad = reverse badR
  let good = reverse goodR
  let -- Split folders and non-folders
  let (folders, tasks) = partition partitionTypes good
  let -- Only tell lsfolder to print a blank line in front of the first folder if items were aren't printed
  let blankToFolder_l = zip ((if null tasks then False else True) : repeat True) folders
  -- Show errors
  liftIO $ mapM_ putStrLn bad
  -- Show non-folder items
  lsitems [] tasks
  -- Show folders
  mapM_ (\(blank, folder) -> lsfolder blank [] folder) blankToFolder_l
  return mempty
  where
    args' = (\x -> if null x then ["."] else x) args0
    chain_l = map (pathStringToPathChain cwd) args'

    partitionArgs :: ([(String, Item)], [String]) -> (String, Validation Item) -> ([(String, Item)], [String])
    partitionArgs (good, bad) (arg, Right item) = ((arg, item):good, bad)
    partitionArgs (good, bad) (arg, Left msgs) = (good, (map (\s -> "ls: cannot access "++arg++": "++s) msgs) ++ bad)

    -- Return true if this if passed a folder or list item (or Nothing item, for the root folder)
    partitionTypes :: (String, Item) -> Bool
    partitionTypes (_, item) = case itemType item of
      "folder" -> True -- TODO: but False if -d
      "list" -> True
      _ -> False

    lsitems :: [FilePath] -> [(String, Item)] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    lsitems chainPrefix nameToItem_l = do
      -- show all the items
      liftIO $ mapM_ (\l -> (putStrLn . intercalate "  ") l) ll
      -- if we want to recurse into folders:
      when isRecursive $ do
        let (folders, _) = partition partitionTypes nameToItem_l
        when ((not . null) folders) $ do
          mapM_ (lsfolder True chainPrefix) folders
      where
        ll = map strings nameToItem_l
        strings (name, item) = case itemType item of
          "folder" -> [ (setSGRCode [SetColor Foreground Vivid Blue]) ++ (joinPath (chainPrefix ++ [name])) ++ "/" ++ (setSGRCode [])]
          "list" -> [(joinPath (chainPrefix ++ [name])) ++ "/"]
          _ -> [(joinPath (chainPrefix ++ [name]))]

    doShowFolderName = isRecursive || length args' > 1

    lsfolder :: Bool -> [FilePath] -> (String, Item) -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
    lsfolder needBlankLine chainPrefix0 (name, item) = do
      let name = itemToName item
      let uuid = itemUuid item
      when needBlankLine (liftIO $ putStrLn "")
      -- TODO: I probably want to use name here
      let chainPrefix = chainPrefix0 ++ [name]
      when doShowFolderName $ liftIO $ do
        setSGR [SetConsoleIntensity BoldIntensity]
        putStrLn ((joinPath chainPrefix)++":")
        setSGR [Reset]
      items_ <- selectList [ItemParent ==. Just uuid] []
      let items = map entityVal items_
      let nameToItem_l = map (\item -> (itemToName item, item)) items
      lsitems chainPrefix nameToItem_l

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
parentToPathChainToItem parent (".":rest) = do
  parentToPathChainToItem parent rest
-- TODO: Handle '..'
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

mkdir :: Env -> ActionMkdir -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
mkdir env action | trace "mkdir" False = undefined
mkdir env@(Env time user cwd) action = do
  if null (mkdirArgs action)
    then return (ActionResult [] False ["mkdir: missing operand", "Try 'mkdir --help' for more information."] [])
    else do

      -- REFACTOR: consider ``mkdir -p a a/b``.  To handle this, we should
      -- build a map of chain to uuids, and add each new folder to the map,
      -- so that we can then also create subfolders all in one go.
      if mkdirParents action
        -- If parent folders should also be created
        then do
          let chain_l = map (pathStringToAbsPathChain (envCwdChain env)) (mkdirArgs action)
          -- Get all unique path chains in sorted order
          let chain_l' = sort $ Set.toList $ Set.delete [] $ Set.fromList $ concat $ map inits chain_l
          result_ <- mapM mkdir' chain_l'
          return (mconcat result_)
        -- Otherwise only create given folders
        else do
          infos0 <- mapM (pathStringToPathInfo env) (mkdirArgs action)
          let infos_ = concatEithersN infos0
          case infos_ of
            -- TODO: rather than returning, we should print errors for the errors and go ahead with the good ones
            Left msgs -> return (ActionResult [] False [] msgs)
            Right infos1 -> do
              -- TODO: remove duplicates
              let infos = sortBy (\a b -> compare (pathInfoChainAbs a) (pathInfoChainAbs b)) infos1
              result_ <- mapM mkdir'' infos
              return (mconcat result_)
  where
    mkdir' :: [FilePath] -> SqlActionResult
    mkdir' [] = return $ (ActionResult [] False [] ["mkdir': empty argument not allowed"])
    mkdir' ["/"] = return mempty
    mkdir' chain = do
      item_ <- absPathChainToItem chain
      case item_ of
        -- Item already exists:
        Right item ->
          -- if --parents flag:
          if mkdirParents action
            then return mempty
            else return (ActionResult [] False [] ["mkdir: cannot create directory ‘"++(show chain)++"’: File exists"])
        -- Item doesn't exist yet:
        Left _ -> do
          parent_ <- absPathChainToItem (init chain)
          case parent_ of
            Left msgs -> return (ActionResult [] False [] msgs)
            Right parent -> do
              uuid <- liftIO (U4.nextRandom >>= return . U.toString)
              let hunk = PatchHunk [uuid] diffs
              return (ActionResult [hunk] False [] [])
              where
                diffs =
                  [ DiffEqual "type" "folder"
                  , DiffEqual "status" "open"
                  , DiffEqual "name" (last chain)
                  , DiffEqual "parent" (itemUuid parent)
                  ]

    mkdir'' :: PathInfo -> SqlActionResult
    mkdir'' info = do
      let chain = pathInfoChainAbs info
      mkdir' chain
--    mkone :: PathInfo -> SqlActionResult
--    -- Item already exists
--    mkone (PathInfo path_s chainRel chainAbs items) = do
--      -- if --parents flag:
--      if mkdirParents action
--        then return mempty
--        else return (ActionResult [] False [] ["mkdir: cannot create directory ‘"++path_s++"’: File exists"])
--    mkone (path_s, (chain, Left chainError)) = do
--      if mkdirParents action
--        then do
--          let more = drop (length chainError) chain
--          
--      <== continue here: if '-parent', create directories from chainError to chain; otherwise if chainError is more than one item shorter than chain, error; otherwise create chain
--      And for each directory created, create a new PatchHunk
--    createFolders chain more 
--
-----------
--  THESE ARE FROM 'ls'; ADAPT!
--  itemTop_ <- mapM absPathChainToItem chain_l
--  let (goodR, badR) = foldl partitionArgs ([], []) (zip args' itemTop_)
--  let bad = reverse badR
--  let good = reverse goodR
--  let -- Split folders and non-folders
--  let (folders, tasks) = partition partitionTypes good
--  let -- Only tell lsfolder to print a blank line in front of the first folder if items were aren't printed
--  let blankToFolder_l = zip ((if null tasks then False else True) : repeat True) folders
--  -- Show errors
--  liftIO $ mapM_ putStrLn bad
--  -- Show non-folder items
--  lsitems [] tasks
--  -- Show folders
--  mapM_ (\(blank, folder) -> lsfolder blank [] folder) blankToFolder_l
--  return mempty
-----------
--
--  if null args
--    then return (ActionResult [] False ["mkdir: missing operand", "Try 'mkdir --help' for more information."] [])
--    else do
--      root_ <- getroot
--      case root_ of
--        Left msgs -> return ([], ActionResult False False [] msgs)
--        Right root -> do
--          chain_l <- mapM
--          result_ <- mapM (mkone root) (mkdirArgs action)
--          --let action' = action { mkdirUuid = 
--          return (mconcat result_)
--  where
--    args = (mkdirArgs action)
--    args' = args ++ (if mkdirParents action then ["--parents"] else [])
--    --record = CommandRecord 1 time (T.pack user) (T.pack "repl-mkdir") (map T.pack args')
--
--    mkone :: Item -> String -> SqlActionResult
--    --mkone path_s | trace ("mkone "++path_s) False = undefined
--    mkone root path_s =
--      let
--        chain = pathStringToPathChain cwd path_s
--        --chains = tail $ inits chain
--      in do
--        -- Check whether the item already exists
--        uuid_ <- pathChainToUuid chain
--        case uuid_ of
--          -- if the item already exists:
--          Right _ -> do
--            -- if --parents flag:
--            if mkdirParents action
--              then return mempty
--              else return (ActionResult False False [] ["mkdir: cannot create directory ‘"++path_s++"’: File exists"])
--          -- if the item doesn't already exist:
--          Left msgs -> do
--            if mkdirParents action
--              -- if all parents should be created
--              then do
--                uuid_ <- fn True chain root
--                case uuid_ of
--                  Left msgs -> return (ActionResult False False [] msgs)
--                  Right uuid -> return (ActionResult True False [] [])
--              -- if all parents should already exist
--              else do
--                parent_ <- fn False (init chain) root
--                case parent_ of
--                  Left msgs -> return (ActionResult False False [] msgs)
--                  Right parent -> do
--                    new_ <- mksub parent (last chain)
--                    case new_ of
--                      Left msgs -> return (ActionResult False False [] msgs)
--                      Right new -> do
--                        return (ActionResult True False [] [])
--
--    fn :: Bool -> [FilePath] -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
--    fn doMake l parent | trace ("fn "++(show l)) False = undefined
--    fn _ [] parent = return (Right parent)
--    fn doMake (name:rest) parent = do
--      item_ <- selectList [ItemLabel ==. (Just name), ItemParent ==. (Just (itemUuid parent))] [LimitTo 2]
--      case item_ of
--        [] -> if doMake
--          then do
--            new_ <- mksub parent name
--            case new_ of
--              Left msgs -> return (Left msgs)
--              Right new -> fn doMake rest new
--          else return (Left ["mkdir: ‘"++name++"’ not found"])
--        p:[] -> fn doMake rest (entityVal p)
--        _ -> return (Left ["conflict: multiple items at path"])
--
--    mksub :: Item -> String -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation Item)
--    mksub parent name = do
--      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
--      let item = (itemEmpty uuid time "folder" name "open") { itemParent = (Just $ itemUuid parent), itemLabel = Just name }
--      insert item
--      return (Right item)

newtask :: Env -> ActionNewTask -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
newtask env action | trace "newtask" False = undefined
newtask (Env time user cwd) action0 = do
  parent_ <- absPathChainToItem parentChain
  case parent_ of
    Left msgs -> return (ActionResult [] False [] msgs)
    Right parent -> do
      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
      let hunk_ = createHunk uuid parent
      case hunk_ of
        Left msgs -> return (ActionResult [] False [] msgs)
        Right hunk -> return (ActionResult [hunk] False [] [])
  where
    parentChain = case newTaskParentRef action0 of
      Just s -> pathStringToPathChain cwd s
      Nothing -> cwd
    get :: (ActionNewTask -> Maybe String) -> Validation String
    get fn = case fn action0 of
      Nothing -> Left ["missing value"]
      Just s -> Right s
    getMaybe :: (ActionNewTask -> Maybe String) -> Validation (Maybe String)
    getMaybe fn = Right (fn action0)
    createHunk :: String -> Item -> Validation PatchHunk
    createHunk uuid parent = do
      status <- getMaybe newTaskStatus
      name <- getMaybe newTaskName
      title <- getMaybe newTaskTitle
      stage <- getMaybe newTaskStage
      let diffs = catMaybes
            [ Just $ DiffEqual "type" "task"
            , Just $ DiffEqual "status" $ fromMaybe "open" status
            , Just $ DiffEqual "parent" (itemUuid parent)
            , fmap (DiffEqual "name") name
            , fmap (DiffEqual "title") title
            , Just $ DiffEqual "stage" $ fromMaybe "new" stage
            ]
      return (PatchHunk [uuid] diffs)
