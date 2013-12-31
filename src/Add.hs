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

module Add
( createAddCommandRecord
, createModCommandRecord
, processCommand_add
, processCommand_mod
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Either
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Time.Clock
import Data.Time.Format (parseTime)
import Data.Time.ISO8601 (formatISO8601Millis, parseISO8601)
import Database.Persist
import Database.Persist.Sqlite
import System.Locale (defaultTimeLocale)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Command as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Console.CmdArgs.Explicit as CmdArgs

import Args
import DatabaseUtils
import DatabaseTables
import Utils

data Arg
  = ArgText String
  | ArgNull String
  | ArgEqual String String
  | ArgAdd String String
  | ArgUnset String
  | ArgRemove String String
  deriving Show

createAddCommandRecord :: UTCTime -> String -> String -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String C.CommandRecord)
createAddCommandRecord time user uuid args0 = do
  x <- parseAddArgs args0
  case x of
    Left msgs -> do return (Left $ unwords msgs)
    Right args -> do
      let args' = (ArgEqual "id" uuid) : args
      return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "add") (argsToTextList args')

createModCommandRecord :: UTCTime -> String -> String -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String C.CommandRecord)
createModCommandRecord time user uuid args0 = do
  x <- parseModArgs args0
  case x of
    Left msgs -> return $ Left $ unwords msgs
    Right args -> do
      let args' = (ArgEqual "id" uuid) : args
      return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "mod") (argsToTextList args')

parseAddArgs :: [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either [String] [Arg])
parseAddArgs args0 = do
  -- Lookup references
  args2 <- mapM refToUuid' args1
  -- Combine text args into a title
  let args3 = combineText args2 "" []
  -- Check for errors
  return (concatEithers1 args3)
  where
    args1 = map parseStringToArg args0
    -- Combine text args into a title
    combineText :: [Either String Arg] -> String -> [Either String Arg] -> [Either String Arg]
    combineText [] "" acc = reverse acc
    combineText [] title acc = (Right $ ArgEqual "title" (strip title)) : (reverse acc)
    combineText ((Right (ArgText s)):xs) title acc = combineText xs (title ++ " " ++ s) acc
    combineText ((Right (ArgEqual "title" s)):xs) title acc = combineText xs (title ++ " " ++ s) acc
    combineText (x:xs) title acc = combineText xs title (x:acc)

parseModArgs :: [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either [String] [Arg])
parseModArgs args0 = do
  -- Lookup references
  args2 <- mapM refToUuid' args1
  -- Check for errors
  case concatEithers1 args2 of
    Left msgs -> return $ Left msgs
    Right args3 -> do
      -- Make sure there are no ArgText args
      case all validate args3 of
        True -> return (Right args3)
        False -> return (Left ["unrecognized argument"])
  where
    args1 = map parseStringToArg args0
    validate (ArgText _) = False
    validate _ = True

parseStringToArg :: String -> Either String Arg
parseStringToArg arg =
  case parseNameOpValue arg of
    Left _ -> Right (ArgText arg)
    Right (name, "=", Nothing) -> Right (ArgNull name)
    Right (name, "=", Just value) -> Right (ArgEqual name value)
    Right (name, "+", Just value) -> Right (ArgAdd name value)
    Right (name, "-", Nothing) -> Right (ArgUnset name)
    Right (name, "-", Just value) -> Right (ArgRemove name value)
    _ -> Left ("Invalid argument: "++arg)

refToUuid' (Right arg) = do refToUuid arg
refToUuid' x = return x

refToUuid :: Arg -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String Arg)
refToUuid (ArgEqual "parent" ref) = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left "Couldn't find parent ref")
    Just uuid -> return (Right $ ArgEqual "parent" uuid)
refToUuid x = return $ Right x

argsToTextList :: [Arg] -> [T.Text]
argsToTextList args = map (T.pack . fn) args where
  fn :: Arg -> String
  fn (ArgEqual name value) = name ++ "=" ++ value
  fn (ArgNull name) = name ++ "="
  fn (ArgAdd name value) = name ++ "+" ++ value
  fn (ArgRemove name value) = name ++ "-" ++ value
  fn (ArgUnset name) = name ++ "-"

argsToMap :: [Arg] -> M.Map String (Maybe String)
argsToMap args = argsToMap' args M.empty where
  argsToMap' :: [Arg] -> M.Map String (Maybe String) -> M.Map String (Maybe String)
  argsToMap' [] m = m
  argsToMap' ((ArgEqual name value):rest) m = argsToMap' rest $ M.insert name (Just value) m
  argsToMap' ((ArgNull name):rest) m = argsToMap' rest $ M.insert name Nothing m
  argsToMap' (_:rest) m = argsToMap' rest m

--preparseAddArgs :: [String] -> (Maybe String, [String]) -> Either String (Maybe String, [String])
--preparseAddArgs l acc = case l of
--  [] -> Right (fst acc, reverse $ snd acc)
--  s : rest ->
--    case parseNameOpValue s of
--      Left _ -> preparseArgs rest acc' where
--        title = case fst acc of
--          Nothing -> Just s
--          Just pre -> Just (pre ++ " " ++ s)
--        acc' = (title, snd acc)
--      Right ("title", "+", Nothing) -> preparseArgs rest acc
--      Right ("title", "+", Just title') -> preparseArgs rest acc' where
--        title = case fst acc of
--          Nothing -> Just s
--          Just pre -> Just (pre ++ " " ++ title')
--        acc' = (title, snd acc)
--      Right ("title", "=", Nothing) -> preparseArgs rest acc' where
--        acc' = (Nothing, snd acc)
--      Right ("title", "=", Just value) -> preparseArgs rest acc' where
--        acc' = (Just value, snd acc)
--      Right ("title", "-", Nothing) -> preparseArgs rest acc' where
--        acc' = (Nothing, snd acc)
--      Right ("title", op, _) -> Left $ "cannot use `"++op++"` operator with `title`"
--      Right (name, op, value) -> preparseArgs rest acc' where
--        acc' = (fst acc, s : snd acc)
--    -- TODO: handle "+home" -> "tag+home", "@home" -> "context+home", "/list" -> "parent=list"

parseArgs :: [String] -> [Either String (String, String, Maybe String)]
parseArgs args = map parseNameOpValue args

lookupRecordProperty :: M.Map String String -> String -> Maybe T.Text
lookupRecordProperty m name =
  M.lookup name m >>= (\x -> Just $ T.pack $ name ++ "=" ++ x)

preparseArgs :: [String] -> [String] -> Either String [String]
preparseArgs l acc = case l of
  [] -> Right $ reverse acc
  s : rest ->
    case parseNameOpValue s of
      Left msg -> Left msg
      Right (name, op, value) -> preparseArgs rest acc' where
        acc' = s : acc


rx = mkRegex "[=+-]"

parseNameOpValue :: String -> Either String (String, String, Maybe String)
parseNameOpValue s = case matchRegexAll rx s of
  Just (name, op, "", _) -> Right (name, op, Nothing)
  Just (name, op, value, _) -> Right (name, op, Just value)
  _ -> Left "missing operator"

-- TODO: Add default title/label for lists
addDefaults :: [Arg] -> [Arg]
addDefaults args = foldl fn args defaults where
  map = argsToMap args
  defaults = [("status", "open"), ("stage", "new")]
  fn :: [Arg] -> (String, String) -> [Arg]
  fn acc (name, value) = case M.lookup name map of
    Nothing -> (ArgEqual name value) : acc
    Just x -> acc


processCommand_add :: UTCTime -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
processCommand_add time args0 = do
  args <- liftIO $ CmdArgs.process args0
  x <- parseAddArgs args0
  case x of
    Left msgs -> return (Left msgs)
    Right args1 -> do
      let args = addDefaults' args1
      let map = argsToMap args
      case M.lookup "id" map of
        Just (Just uuid) -> do
          case createItem time map of
            Left msgs -> return (Left msgs)
            Right item -> do
              insert item
              mapM_ (saveProperty uuid) args
              return $ Right ()
        Nothing -> return (Left ["Missing `id`"])

addDefaults' :: Options -> Options
addDefaults' args = args' where
  defaults = [("status", "open"), ("stage", "new")]
  flags' = foldl fn (argumentsFlags args) defaults where
  map = argsToMap args
  fn :: [Arg] -> (String, String) -> [Arg]
  fn acc (name, value) = case M.lookup name map of
    Nothing -> (ArgEqual name value) : acc
    Just x -> acc

processCommand_mod :: UTCTime -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Validation ())
processCommand_mod time args0 = do
  x <- parseModArgs args0
  case x of
    Left msgs -> return (Left msgs)
    Right args -> do
      let map = argsToMap args
      case M.lookup "id" map of
        Just (Just uuid) -> do
          entity' <- getBy $ ItemUniqUuid uuid
          case entity' of
            Nothing -> return (Left ["Could not find item with given id"])
            Just entity -> do
              let item0 = entityVal entity
              case updateItem time map item0 of
                Nothing -> return (Left ["Couldn't update item"])
                Just item -> do
                  replace (entityKey entity) item
                  mapM_ (saveProperty uuid) args
                  return (Right ())
        Nothing -> return (Left ["Missing `id`"])

itemFields = ["id", "type", "title", "status", "parent", "stage", "label", "index"]

--instance Monad (Either e) where
--  (Left msgs) >>= f = Left msgs
--  (Right x) >>= f = f x
--  return = Right

createItem :: UTCTime -> M.Map String (Maybe String) -> Either [String] Item
createItem time map = do
  id <- get "id"
  type_ <- get "type"
  title <- get "title"
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
  return $ Item id time type_ title status parent stage label Nothing closed start end due review
  where
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

updateItem :: UTCTime -> M.Map String (Maybe String) -> Item -> Maybe Item
updateItem time map item0 =
  Item <$>
    get "id" itemUuid <*>
    Just (itemCtime item0) <*>
    get "type" itemType <*>
    get "title" itemTitle <*>
    get "status" itemStatus <*>
    getMaybe "parent" itemParent <*>
    getMaybe "stage" itemStage <*>
    getMaybe "label" itemLabel <*>
    Just (itemIndex item0) <*>
    getMaybeDate "closed" itemClosed <*>
    getMaybeDate "start" itemStart <*>
    getMaybeDate "end" itemEnd <*>
    getMaybeDate "due" itemDue <*>
    getMaybeDate "review" itemReview
  where
    get :: String -> (Item -> String) -> Maybe String
    get name fn = case M.lookup name map of
      Just (Just s) -> Just s
      _ -> Just (fn item0)

    getMaybe :: String -> (Item -> Maybe String) -> Maybe (Maybe String)
    getMaybe name fn = case M.lookup name map of
      Just (Just s) -> Just (Just s)
      _ -> Just (fn item0)

    getMaybeDate :: String -> (Item -> Maybe UTCTime) -> Maybe (Maybe UTCTime)
    getMaybeDate name fn = case M.lookup name map of
      Just (Just s) -> case parseTime defaultTimeLocale "%Y%m%dT%H%M%S" s of
        Just time -> Just (Just time)
        Nothing -> Nothing
      _ -> Just (fn item0)

saveProperty :: String -> Arg -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
saveProperty uuid (ArgEqual name value) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
    insert $ Property "item" uuid name value
    return ()
saveProperty uuid (ArgUnset name) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
    return ()
saveProperty uuid (ArgAdd name value) = do
  if elem name itemFields then return () else do
    insert $ Property "item" uuid name value
    return ()
saveProperty uuid (ArgRemove name value) = do
  if elem name itemFields then return () else do
    deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name, PropertyValue ==. value]
    return ()
saveProperty _ arg = do
  liftIO $ print $ "Don't know how to handle arg: " ++ (show arg)

