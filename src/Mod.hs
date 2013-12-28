module Mod
( createModCommandRecord
, processModCommand
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import DatabaseTables
import qualified Data.Map as M
import Database.Persist
import Database.Persist.Sqlite
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601 (formatISO8601Millis)
import Text.Regex (mkRegex, matchRegexAll)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Command as C
import qualified Data.Text as T
import Database

createModCommandRecord :: UTCTime -> String -> String -> [String] -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String C.CommandRecord)
createModCommandRecord time user uuid args =
  case args of
    ref : args' -> do
      uuid' <- databaseLookupUuid ref
      case uuid' of
        Nothing -> return $ Left $ "undefined reference `" ++ ref ++ "`"
        Just uuid ->
          case preparseArgs args [] of
            Left msg -> return $ Left msg
            Right args' -> return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "mod") (l1 ++ l2) where
              xs = parseArgs args'
              map1 = makeMap xs
              l1 = catMaybes $ map (lookupRecordProperty map1) ["type", "title", "stage", "status"]
              l2 = catMaybes $ map (fn "tag") xs ++ map (fn "context") xs
              fn :: String -> Either String (String, String, Maybe String) -> Maybe T.Text
              fn name (Right (name', op, Just value)) = if name' == name then Just (T.pack $ name ++ op ++ value) else Nothing
              fn name (Right (name', "-", Nothing)) = if name' == name then Just (T.pack $ name ++ "-") else Nothing
              fn _ _ = Nothing
    _ -> return $ Left "must specify reference to an item"

lookupRecordProperty :: M.Map String String -> String -> Maybe T.Text
lookupRecordProperty m name =
  M.lookup name m >>= (\x -> Just $ T.pack $ name ++ "=" ++ x)

preparseArgs :: [String] -> [String] -> Either String [String]
preparseArgs l acc = case l of
  [] -> Right $ reverse acc
  s : rest ->
    case parse s of
      Left msg -> Left msg
      Right (name, op, value) -> preparseArgs rest acc' where
        acc' = s : acc
    -- TODO: handle "+home" -> "tag+home", "@home" -> "context+home", "/list" -> "parent=list"

parseArgs :: [String] -> [Either String (String, String, Maybe String)]
parseArgs args = map parse args

rx = mkRegex "[=+-]"

parse :: String -> Either String (String, String, Maybe String)
parse s = case matchRegexAll rx s of
  Just (name, op, "", _) -> Right (name, op, Nothing)
  Just (name, op, value, _) -> Right (name, op, Just value)
  _ -> Left "missing operator"

makeMap :: [Either String (String, String, Maybe String)] -> M.Map String String
makeMap xs = M.fromList $ catMaybes $ map fn xs where
  fn (Right (name, "=", Just value)) = Just (name, value)
  fn _ = Nothing

processModCommand :: (PersistQuery m, PersistStore m) => UTCTime -> [String] -> m ()
processModCommand time args = do
  let xs = parseArgs args
  let map = makeMap xs
  case M.lookup "id" map of
    Nothing -> return ()
    Just uuid -> do
      -- if this item hasn't been created yet, set the creation time
      one <- selectList [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. "ctime"] [LimitTo 1]
      when (null one) $ do
        insert $ Property "item" uuid "ctime" (formatISO8601Millis time)
        return ()
      -- Update the other properties
      mapM_ fn xs
      where
        fn (Right x) = processItem uuid x
        fn (Left msg) = liftIO $ putStrLn msg

processItem :: (PersistQuery m, PersistStore m) => String -> (String, String, Maybe String) -> m ()
processItem uuid (name, "=", Just value) = do
  deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
  insert $ Property "item" uuid name value
  return ()
processItem uuid (name, "-", Just value) = do
  deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name, PropertyValue ==. value]
  return ()
processItem uuid (name, "-", Nothing) = do
  deleteWhere [PropertyTable ==. "item", PropertyUuid ==. uuid, PropertyName ==. name]
  return ()
processItem uuid (name, "+", Just value) = do
  insert $ Property "item" uuid name value
  return ()

