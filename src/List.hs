module List
( listHandler
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Database.Persist (PersistQuery)
import Database.Persist.Sql (insert, deleteWhere)
--import Database.Persist.Sqlite
import Database.Esqueleto
import Text.Regex (mkRegex, matchRegexAll)

import qualified Data.Map as M
import qualified Command as C
import qualified Data.Text as T

import DatabaseTables

type PropertyMap = M.Map String [String]
type EntityMap = M.Map String PropertyMap

listHandler :: [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
listHandler args = do
  items0 <- select $ from $ \t -> do
    where_ (isNothing $ t ^. ItemParent)
    return t
  entities <- select $ from $ \t -> do
    return t
  let properties = map entityVal entities
  let m = fn1' properties
  --mapM_ (\entity -> processCommand (entityVal entity)) l
  mapM_ (\entity -> printItem m 0 (entityVal entity)) items0
  --liftIO $ mapM_ (\entity -> print $ entityVal entity) $ entities
  --liftIO $ mapM_ (\(uuid, properties) -> putStrLn $ itemToString uuid properties m) $ M.toList m
  return ()

--printItem :: EntityMap -> Int -> Item -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
printItem entities indent item = do
  let indent_s = (replicate indent ' ')
  --liftIO $ putStrLn $ indent_s ++ show item
  liftIO $ putStrLn $ indent_s ++ itemToString' item entities
  items <- select $ from $ \t -> do
    where_ (t ^. ItemParent ==. val (Just $ itemUuid item))
    return t
  mapM_ (\entity -> printItem entities (indent + 2) (entityVal entity)) items

fn1' :: [Property] -> EntityMap
fn1' properties = fn1 properties M.empty

fn1 :: [Property] -> EntityMap -> EntityMap
fn1 [] acc = acc
fn1 ((Property table uuid name value) : rest) acc = fn1 rest acc' where
  acc' = case M.lookup uuid acc of
    Nothing -> M.insert uuid M.empty acc
    Just m -> M.insert uuid m' acc where
      m' = M.insert name [value] m

findParentLabel :: Maybe [String] -> EntityMap -> [String]
findParentLabel uuid m = findParentLabel' uuid m []

findParentLabel' :: Maybe [String] -> EntityMap -> [String] -> [String]
findParentLabel' (Just [uuid]) m acc =
  -- See whether entity exists
  case M.lookup uuid m of
    Nothing -> acc
    Just m' -> findParentLabel' uuid' m acc' where
      -- Update acc with label or title
      acc' = case M.lookup "label" m' of
        Nothing ->
          case M.lookup "title" m' of
            Nothing -> acc
            Just title -> acc ++ title
        Just label -> acc ++ label
      -- Continuing by search for this items parent
      uuid' = M.lookup "parent" m'
findParentLabel' _ _ acc = acc

itemToString' :: Item -> EntityMap -> String
itemToString' item entities = unwords l where
  path = findParentLabel (itemParent item >>= \x -> Just [x]) entities
  check :: Maybe String
  check = case (itemType item, itemStatus item) of
    ("list", "open") -> Nothing
    ("list", "closed") -> Just "[x]"
    ("list", "deleted") -> Just "XXX"
    ("task", "open") -> Just "- [ ]"
    (_, "open") -> Just "- "
    (_, "closed") -> Just "- [x]"
    (_, "deleted") -> Just "- XXX"
    _ -> Nothing
  l :: [String]
  l = catMaybes $
    [ check
    , itemIndex item >>= (\x -> Just ("(" ++ (show x) ++ ")"))
    , if null path then Nothing else Just $ intercalate "/" path ++ ":"
    , Just $ itemTitle item
    ]

itemToString :: String -> PropertyMap -> EntityMap -> String
itemToString uuid properties entities = unwords l where
  path = findParentLabel (M.lookup "parent" properties) entities
  isTask :: Bool
  isTask = (M.lookup "type" properties) == Just ["task"]
  check :: String
  check = case M.lookup "status" properties of
    Just ["open"] -> if isTask then " [ ]" else ""
    Just ["closed"] -> " [x]"
    Just ["deleted"] -> " XXX"
    _ -> ""
  l :: [String]
  l = catMaybes $
    [ Just $ "-" ++ check
    , M.lookup "index" properties >>= (\x -> Just ("(" ++ unwords x ++ ")"))
    , if null path then Nothing else Just $ intercalate "/" path ++ ":"
    , M.lookup "title" properties >>= Just . unwords
    ]