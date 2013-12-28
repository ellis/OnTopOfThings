module Add
( processAddCommand
) where

import DatabaseTables
import qualified Data.Map as M
import           Database.Persist
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Text.Regex (mkRegex, matchRegexAll)
import           Control.Monad.IO.Class  (liftIO)
import qualified Command as C
import qualified Data.Text as T

createAddCommandRecord :: (PersistQuery m, PersistStore m) => UTCTime -> String -> String -> [String] -> m (Either String C.CommandRecord)
createAddCommandRecord time user uuid args = do
  return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "add") (l1 ++ l2) where
    xs = parseArgs args
    map0 = makeMap xs
    map1 = M.unions [M.fromList [("id", uuid)], map0, M.fromList [("type", "task"), ("stage", "inbox")]]
    l1 = catMaybes
      [ Just (T.pack $ "id=" ++ uuid)
      , M.lookup "type" map1 >>= (\x -> Just $ T.pack $ "type=" ++ x)
      , M.lookup "stage" map1 >>= (\x -> Just $ T.pack $ "stage=" ++ x)
      ]
    l2 = catMaybes $ map fn xs
    fn :: Either String (String, String, Maybe String) -> Maybe T.Text
    fn (Right (name, op, Just value)) = if name == "tag" then Just (T.pack $ name ++ op ++ value) else Nothing
    fn (Right (name, "-", Nothing)) = if name == "tag" then Just (T.pack $ name ++ "-") else Nothing
    fn _ = Nothing

processAddCommand :: (PersistQuery m, PersistStore m) => [String] -> m ()
processAddCommand args = do
  let xs = parseArgs args
  let map = makeMap xs
  case M.lookup "id" map of
    Nothing -> return ()
    Just uuid -> do
      mapM_ fn xs
      where
        fn (Right x) = processItem uuid x
        fn (Left msg) = liftIO $ putStrLn msg

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

