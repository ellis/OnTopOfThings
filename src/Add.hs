module Add
( createAddCommandRecord
, processAddCommand
, processModCommand
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Map as M
import Database.Persist
import Database.Persist.Sqlite
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import Text.Regex (mkRegex, matchRegexAll)
import qualified Command as C
import qualified Data.Text as T

import DatabaseUtils
import DatabaseTables

createAddCommandRecord :: (PersistQuery m, PersistStore m) => UTCTime -> String -> String -> [String] -> m (Either String C.CommandRecord)
createAddCommandRecord time user uuid args =
  case preparseArgs args (Nothing, []) of
    Left msg -> return $ Left msg
    Right (Nothing, _) -> return $ Left "you must specify a title"
    Right (Just title, args') -> return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "add") l where
      xs = parseArgs args'
      l = (T.pack $ "id=" ++ uuid) : (catMaybes $ map fn xs)
      fn :: Either String (String, String, Maybe String) -> Maybe T.Text
      fn (Right (name, op, Just value)) = Just (T.pack $ name ++ op ++ value)
      fn (Right (name, "-", Nothing)) = Just (T.pack $ name ++ "-")
      fn _ = Nothing

preparseArgs :: [String] -> (Maybe String, [String]) -> Either String (Maybe String, [String])
preparseArgs l acc = case l of
  [] -> Right (fst acc, reverse $ snd acc)
  s : rest ->
    case parse s of
      Left _ -> preparseArgs rest acc' where
        title = case fst acc of
          Nothing -> Just s
          Just pre -> Just (pre ++ " " ++ s)
        acc' = (title, snd acc)
      Right ("title", "+", Nothing) -> preparseArgs rest acc
      Right ("title", "+", Just title') -> preparseArgs rest acc' where
        title = case fst acc of
          Nothing -> Just s
          Just pre -> Just (pre ++ " " ++ title')
        acc' = (title, snd acc)
      Right ("title", "=", Nothing) -> preparseArgs rest acc' where
        acc' = (Nothing, snd acc)
      Right ("title", "=", Just value) -> preparseArgs rest acc' where
        acc' = (Just value, snd acc)
      Right ("title", "-", Nothing) -> preparseArgs rest acc' where
        acc' = (Nothing, snd acc)
      Right ("title", op, _) -> Left $ "cannot use `"++op++"` operator with `title`"
      Right (name, op, value) -> preparseArgs rest acc' where
        acc' = (fst acc, s : snd acc)
    -- TODO: handle "+home" -> "tag+home", "@home" -> "context+home", "/list" -> "parent=list"

parseArgs :: [String] -> [Either String (String, String, Maybe String)]
parseArgs args = map parse args

rx = mkRegex "[=+-]"

parse :: String -> Either String (String, String, Maybe String)
parse s = case matchRegexAll rx s of
  Just (name, op, "", _) -> Right (name, op, Nothing)
  Just (name, op, value, _) -> Right (name, op, Just value)
  _ -> Left "missing operator"

refsToUuids :: [Either String (String, String, Maybe String)] -> SqlPersistT (NoLoggingT (ResourceT IO)) [Either String (String, String, Maybe String)]
refsToUuids xs = mapM refToUuid xs

refToUuid :: Either String (String, String, Maybe String) -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (String, String, Maybe String))
refToUuid (Right ("parent", "=", Just ref)) = do
  uuid' <- databaseLookupUuid ref
  case uuid' of
    Nothing -> return (Left "Couldn't find parent ref")
    Just uuid -> return (Right ("parent", "=", Just uuid))
refToUuid x = return x

makeMap :: [Either String (String, String, Maybe String)] -> M.Map String String
makeMap xs = M.fromList $ catMaybes $ map fn xs where
  fn (Right (name, "=", Just value)) = Just (name, value)
  fn _ = Nothing

addDefaults :: M.Map String String -> [Either String (String, String, Maybe String)] -> [Either String (String, String, Maybe String)]
addDefaults map xs = foldl fn xs defaults where
  defaults = [("status", "open"), ("stage", "inbox")]
  --fn :: [Either String (String, String, Maybe String)] -> (String, String) ->
  fn acc (name, value) = case M.lookup name map of
    Nothing -> (Right (name, "=", Just value)) : acc
    Just x -> acc

processAddCommand :: UTCTime -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
processAddCommand time args = do
  let xs' = parseArgs args
  xs'' <- refsToUuids xs'
  let map' = makeMap xs''
  let xs = addDefaults map' xs''
  let map = makeMap xs
  case M.lookup "id" map of
    Nothing -> liftIO $ putStrLn "Missing `id`"
    Just uuid -> do
      case createItem time map of
        Just item -> do
          -- Create Item
          insert item
          -- Update the other properties
          mapM_ fn xs
          return ()
          where
            fn (Right x) = processItem uuid x
            fn (Left msg) = liftIO $ putStrLn msg
        Nothing -> liftIO $ putStrLn "Couldn't construct Item"

processModCommand :: UTCTime -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
processModCommand time args = do
  let xs = parseArgs args
  let map = makeMap xs
  case M.lookup "id" map of
    Nothing -> do liftIO $ putStrLn "Missing `id`"
    Just uuid -> do
      entity' <- getBy $ ItemUniqUuid uuid
      case entity' of
        Nothing -> do liftIO $ putStrLn "Could not find item with given id"
        Just entity -> do
          let item0 = entityVal entity
          case updateItem time map item0 of
            Nothing -> do liftIO $ putStrLn "Couldn't update item"
            Just item -> do
              replace (entityKey entity) item
              mapM_ fn xs
              where
                fn (Right x) = processItem uuid x
                fn (Left msg) = liftIO $ putStrLn msg


itemFields = ["id", "type", "title", "status", "parent", "stage", "label", "index"]

createItem :: UTCTime -> M.Map String String -> Maybe Item
createItem time map = Item <$> M.lookup "id" map <*> Just time <*> M.lookup "type" map <*> M.lookup "title" map <*> M.lookup "status" map <*> Just (M.lookup "parent" map) <*> Just (M.lookup "stage" map) <*> Just (M.lookup "label" map) <*> Just Nothing

updateItem :: UTCTime -> M.Map String String -> Item -> Maybe Item
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
    Just (itemIndex item0)
  where
    get :: String -> (Item -> String) -> Maybe String
    get name fn = case M.lookup name map of
      Nothing -> Just (fn item0)
      Just s -> Just s

    getMaybe :: String -> (Item -> Maybe String) -> Maybe (Maybe String)
    getMaybe name fn = case M.lookup name map of
      Nothing -> Just (fn item0)
      Just s -> Just (Just s)

processItem :: (PersistQuery m, PersistStore m) => String -> (String, String, Maybe String) -> m ()
processItem uuid (name, op, value') =
  if elem name itemFields
  then return ()
  else
    case (op, value') of
      ("=", Just value) -> do
        deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
        insert $ Property "item" uuid name value
        return ()
      ("-", Just value) -> do
        deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name, PropertyValue ==. value]
        return ()
      ("-", Nothing) -> do
        deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
        return ()
      ("+", Just value) -> do
        insert $ Property "item" uuid name value
        return ()
      _ -> return ()

