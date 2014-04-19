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
{-# LANGUAGE MultiWayIf #-}

module OnTopOfThings.Actions.Run where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (inits, intercalate, partition, sort, sortBy)
import Data.Maybe
import Data.Monoid
import Data.Time (TimeZone, UTCTime, getCurrentTime, getCurrentTimeZone, utc)
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
import OnTopOfThings.Actions.Utils
import OnTopOfThings.Commands.Show
--import OnTopOfThings.Data.DatabaseJson
import OnTopOfThings.Data.FileJson
import OnTopOfThings.Data.Patch
import OnTopOfThings.Data.PatchDatabase
import OnTopOfThings.Data.Time


instance Action ActionCat where
  runAction env action = cat env action >>= \result -> return (env, result)
  actionFromOptions env opts = do
    let refs_ = concatEithersN $ map parseNumberList (optionsArgs opts)
    case refs_ of
      Left msgs -> return (Left msgs)
      Right refs' -> do
        let refs = concat refs'
        items_ <- mapM (lookupItem env) refs
        case concatEithersN items_ of
          Left msgs -> return (Left msgs)
          Right items -> return (Right (ActionCat uuids)) where
            uuids = map itemUuid items
  actionToRecordArgs action = Nothing

instance Action ActionClose where
  runAction env action = close env action >>= \result -> return (env, result)
  actionFromOptions env opts = do
    let refs_ = concatEithersN $ map parseNumberList (optionsArgs opts)
    case refs_ of
      Left msgs -> return (Left msgs)
      Right refs' -> do
        let refs = concat refs'
        items_ <- mapM (lookupItem env) refs
        case concatEithersN items_ of
          Left msgs -> return (Left msgs)
          Right items -> return (Right (ActionClose uuids delete)) where
            uuids = map itemUuid items
            delete = Set.member "delete" (optionsParams0 opts)
  actionToRecordArgs action = Nothing

instance Action ActionLs where
  runAction env action = ls env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionLs)
  actionFromOptions env opts = let
      args = optionsArgs opts
      isRecursive = Set.member "recursive" (optionsParams0 opts)
    in do
      return (Right (ActionLs args isRecursive))

  actionToRecordArgs action = Nothing

instance Action ActionMkdir where
  runAction env action = mkdir env action >>= \result -> return (env, result)
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionMkdir)
  actionFromOptions env opts = do
    return (Right (ActionMkdir (optionsArgs opts) (M.lookup "id" (optionsParams1 opts)) (Set.member "parents" (optionsParams0 opts))))
  actionToRecordArgs action = Just $ flags ++ args where
    args = mkdirArgs action
    flags = catMaybes
      [ mkdirUuid action >>= \x -> Just ("--id="++x)
      , if mkdirParents action then Just "--parents" else Nothing
      ]

instance Action ActionNewTask where
  runAction env action = newtask env action
  --actionFromOptions :: Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ActionMkdir)
  actionFromOptions env opts = do
    tz <- liftIO $ getCurrentTimeZone
    let action_ = createAction tz
    return action_
    where
      createAction :: TimeZone -> Validation ActionNewTask
      createAction tz = do
        id <- getMaybe "id"
        title <- get "title"
        type_ <- getMaybe "type"
        status <- getMaybe "status"
        parent <- getMaybe "parent"
        stage <- getMaybe "stage"
        name <- getMaybe "name"
        closed <- getMaybeDate "closed" tz
        start <- getMaybeDate "start" tz
        end <- getMaybeDate "end" tz
        due <- getMaybeDate "due" tz
        review <- getMaybeDate "review" tz
        return $ ActionNewTask
          { newTaskType = type_
          , newTaskUuid = id
          , newTaskParentRef = parent
          , newTaskName = name
          , newTaskTitle = (if type_ == Just "show" then Nothing else Just title)
          , newTaskContent = (if type_ == Just "show" then Just title else Nothing)
          , newTaskStatus = status
          , newTaskStage = stage
          , newTaskStart = start
          , newTaskEnd = end
          , newTaskDue = due
          , newTaskTags = fromMaybe [] $ M.lookup "tag" (optionsParamsN opts)
          }
      map = optionsMap opts
      get name = case M.lookup name map of
        Just (Just x) -> Right x
        _ -> Left ["missing value for `" ++ name ++ "`"]
      getMaybe name = case M.lookup name map of
        Just (Just s) -> Right (Just s)
        _ -> Right Nothing
      getMaybeDate :: String -> TimeZone -> Validation (Maybe Time)
      getMaybeDate name tz = case M.lookup name map of
        Just (Just s) -> parseTime' tz s >>= \time -> Right (Just time)
        _ -> Right Nothing

  actionToRecordArgs action = Nothing

instance Action ActionShow where
  runAction env action = show' env (showOptions action) >>= \result -> return (env, result)
  actionFromOptions env opts = return (Right (ActionShow opts))
  actionToRecordArgs action = Nothing


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

mode_close = Mode
  { modeGroupModes = mempty
  , modeNames = ["close"]
  , modeValue = options_empty "close"
  , modeCheck = Right
  , modeReform = Just . reform
  , modeExpandAt = True
  , modeHelp = "Mark an item as closed"
  , modeHelpSuffix = []
  , modeArgs = ([], Just (flagArg updArgs "FILE"))
  , modeGroupFlags = toGroup
    [ flagNone ["delete", "d"] (upd0 "delete") "Mark the FILE(s) as deleted rather than closed"
    , flagNone ["help"] updHelp "display this help and exit"
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
  , modeValue = options_empty "newtask"
  , modeCheck = Right
  , modeReform = const Nothing
  , modeExpandAt = True
  , modeHelp = "Add a new task"
  , modeHelpSuffix = []
  , modeArgs = ([flagArg (upd1 "title") "TITLE"], Just (flagArg updNewTaskOption "OPTION"))
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (upd1 "parent") "ID" "reference to parent of this item"
    --, flagReq ["closed"] (upd1 "closed") "TIME" "Time that this item was closed."
    , flagReq ["id"] (upd1 "id") "ID" "A unique ID for this item.  Normally this is randomly assigned."
    , flagReq ["type"] (upd1 "type") "TYPE" "task|note|folder. (default=task)"
    , flagReq ["name", "n"] (upd1 "name") "NAME" "A unique label for this item."
    , flagReq ["stage", "s"] (upd1 "stage") "STAGE" "inbox|today|next|week|month|quarter|year. (default=inbox)"
    , flagReq ["status"] (upd1 "status") "STATUS" "open|closed|deleted. (default=open)"
    , flagReq ["start"] (upd1 "start") "TIME" "Start time"
    , flagReq ["end"] (upd1 "end") "TIME" "End time"
    , flagReq ["due"] (upd1 "due") "TIME" "Due time"
    , flagReq ["tag", "t"] (updN "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["title"] (upd1 "title") "TITLE" "Title of the item."
    --, flagReq ["type"] (upd1 "type") "TYPE" "list|task. (default=task)"
    --, flagReq ["newfolder", "F"] (upd1 "newfolder") "FOLDER" "Place item in folder, and create the folder if necessary."
    , flagHelpSimple updHelp
    ]
  }

-- parameter shortcuts (/work = -p work, +mustdo = -t mustdo, *next = -s next, "tomorrow 15h")
updNewTaskOption :: String -> Options -> Either String Options
updNewTaskOption s opts = case s of
  '/':x -> upd1 "parent" x opts
  '+':x -> updN "tag" x opts
  '-':_ -> updN "tag" s opts
  '?':x -> upd1 "stage" x opts
  _ -> Left ("unrecognized option: "++s)

cat :: Env -> ActionCat -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
cat env action | trace ("cat: "++(show action)) False = undefined
cat (Env time user cwd stage indexNext) action@(ActionCat args0) = do
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

close :: Env -> ActionClose -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
close env (ActionClose uuids delete) = do
  let diffs =
        [ DiffEqual "status" (if delete then "deleted" else "closed")
        , DiffEqual "closed" (formatISO8601 (envTime env))
        ]
  let hunk = PatchHunk uuids diffs
  return (ActionResult [hunk] False [] [])
  where

ls :: Env -> ActionLs -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
ls env action | trace ("ls: "++(show action)) False = undefined
ls (Env time user cwd stage indexNext) action@(ActionLs args0 isRecursive) = do
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

mkdir :: Env -> ActionMkdir -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
mkdir env action | trace "mkdir" False = undefined
mkdir env@(Env time user cwd stage indexNext) action = do
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

newtask :: Env -> ActionNewTask -> SqlPersistT (NoLoggingT (ResourceT IO)) (Env, ActionResult)
newtask env0 action | trace "newtask" False = undefined
newtask env0@(Env time user cwd stage indexNext) action0 = do
  parent_ <- absPathChainToItem parentChain
  case parent_ of
    Left msgs -> return (env0, ActionResult [] False [] msgs)
    Right parent -> do
      uuid <- liftIO (U4.nextRandom >>= return . U.toString)
      let hunk_ = createHunk uuid parent
      case hunk_ of
        Left msgs -> return (env0, ActionResult [] False [] msgs)
        Right hunk -> return (env0 { envIndexNext = envIndexNext env0 + 1 }, ActionResult [hunk] False [] [])
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
    getMaybeTime :: (ActionNewTask -> Maybe Time) -> Validation (Maybe String)
    getMaybeTime fn = Right (flip fmap (fn action0) formatTime')
    createHunk :: String -> Item -> Validation PatchHunk
    createHunk uuid parent = do
      type_' <- getMaybe newTaskType
      status' <- getMaybe newTaskStatus
      name <- getMaybe newTaskName
      title <- getMaybe newTaskTitle
      content <- getMaybe newTaskContent
      start <- getMaybeTime newTaskStart
      end <- getMaybeTime newTaskEnd
      due <- getMaybeTime newTaskDue
      let type_ = fromMaybe "task" type_'
      let status = fromMaybe "open" status'
      stage <- getStage type_ status start due
      let tags = flip map (newTaskTags action0) (\s -> if take 1 s == "-" then DiffRemove "tag" (tail s) else DiffAdd "tag" s)
      let diffs = catMaybes
            [ Just $ DiffEqual "type" type_
            , Just $ DiffEqual "status" status
            , Just $ DiffEqual "parent" (itemUuid parent)
            , fmap (DiffEqual "name") name
            , fmap (DiffEqual "title") title
            , fmap (DiffEqual "content") title
            , fmap (DiffEqual "stage") stage
            , fmap (DiffEqual "start") start
            , fmap (DiffEqual "end") end
            , fmap (DiffEqual "due") due
            , Just $ DiffEqual "index" (show $ envIndexNext env0)
            ]
            ++ tags
      return (PatchHunk [uuid] diffs)
    getStage :: String -> String -> Maybe String -> Maybe String -> Validation (Maybe String)
    getStage type_ status start due =
      if  | isJust (newTaskStage action0) -> Right (newTaskStage action0)
          -- Closed items don't need a stage
          | status /= "open" -> Right Nothing
          -- Folders and notes don't need a stage
          | elem type_ ["folder", "note", "show"] -> Right Nothing
          -- | elem type_ ["goal", "project", "task"] =
          | otherwise -> Right (Just "inbox")

show' :: Env -> Options -> SqlPersistT (NoLoggingT (ResourceT IO)) (ActionResult)
show' env opts = do
  liftIO $ optsRun_show opts
  return mempty
