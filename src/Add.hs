module Add
( processAddCommand
, createAddCommandRecord
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
createAddCommandRecord time user uuid args =
    case preparseArgs args (Nothing, []) of
      Left msg -> return $ Left msg
      Right (Nothing, _) -> return $ Left "you must specify a title"
      Right (Just title, args') -> return $ Right $ C.CommandRecord 1 time (T.pack user) (T.pack "add") (l1 ++ l2) where
        xs = parseArgs args'
        map0 = makeMap xs
        map1 = M.union map0 (M.fromList [("type", "task"), ("stage", "inbox")])
        l1 = catMaybes
          [ Just (T.pack $ "id=" ++ uuid)
          , M.lookup "type" map1 >>= (\x -> Just $ T.pack $ "type=" ++ x)
          , Just (T.pack $ "title=" ++ title)
          , M.lookup "stage" map1 >>= (\x -> Just $ T.pack $ "stage=" ++ x)
          ]
        l2 = catMaybes $ map (fn "tag") xs ++ map (fn "context") xs
        fn :: String -> Either String (String, String, Maybe String) -> Maybe T.Text
        fn name (Right (name', op, Just value)) = if name' == name then Just (T.pack $ name ++ op ++ value) else Nothing
        fn name (Right (name', "-", Nothing)) = if name' == name then Just (T.pack $ name ++ "-") else Nothing
        fn _ _ = Nothing

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
    -- TODO: handle "+home" -> "tag+home", "@home" -> "context+home"

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

