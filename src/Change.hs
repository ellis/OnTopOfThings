{-# LANGUAGE OverloadedStrings #-}

module Change
( ChangeRecord(..)
, ChangeEntity(..)
, ChangeProperty(..)
, DB
, readChangeRecord
, loadDB
, testChangeRecord
) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Text (Text, pack, unpack)
import Text.Regex (mkRegex, matchRegexAll)

data ChangeRecord = ChangeRecord
  { changeFormat :: Int
  , changeTime :: !Text
  , changeEntities :: [ChangeEntity]
  } deriving (Show)

data ChangeEntity = ChangeEntity
  { changeTable :: !Text
  , changeId :: !Text
  , changeProperties :: [ChangeProperty]
  } deriving (Show)

data ChangeProperty = ChangeProperty !Text !Text !Text
  deriving (Show)

instance FromJSON ChangeRecord where
  parseJSON (Object v) = ChangeRecord <$> format <*>  time <*> list where
    format = v .: "format"
    time = v .: "time"
    list = case HM.lookup "list" v of
      Just a -> parseJSON a
      Nothing -> ((parseJSON (Object v)) :: Parser ChangeEntity) >>= (\x -> return [x])
  parseJSON _ = empty

instance FromJSON ChangeEntity where
  parseJSON (Object v) = ChangeEntity <$> table <*> id <*> properties where
    table = v .:? "table" .!= "item"
    id = v .: "id"
    properties = v .: "properties"
  parseJSON _ = empty

instance FromJSON ChangeProperty where
  parseJSON (String v) = result where
    rx = mkRegex "[=+-]"
    result = case matchRegexAll rx (unpack v) of
      Just (name, op, value, _) ->
        return $ ChangeProperty (pack name) (pack op) (pack value)
      _ -> empty
  parseJSON (Array v) = ChangeProperty <$> name <*> op <*> value where
    name = parseJSON (v V.! 0)
    op = parseJSON (v V.! 1)
    value = parseJSON (v V.! 2)
  parseJSON _ = empty

readChangeRecord :: String -> IO (Either String ChangeRecord)
readChangeRecord filename = do
  (eitherDecode <$> B.readFile filename) :: IO (Either String ChangeRecord)

testChangeRecord :: IO ()
testChangeRecord = do
  d <- readChangeRecord "testdata/change001.json"
  putStrLn $ show d
  case d of
    Right file -> do
      let db0 = M.empty :: DB
      let db = processChangeRecord file db0
      let l = M.toList db
      mapM_ (\(k, v) -> putStrLn $ (show k) ++ (show v)) l

loadDB :: IO DB
loadDB = do
  d <- readChangeRecord "testdata/change001.json"
  case d of
    Right file -> do
      let db0 = M.empty :: DB
      return $ processChangeRecord file db0

type DB = M.Map (Text, Text, Text) Value

processChangeRecord :: ChangeRecord -> DB -> DB
processChangeRecord record db = db'' where
  entities = changeEntities record
  db'' = foldl (\db' entity -> processChangeEntity (changeTime record) entity db') db entities

processChangeEntity :: Text -> ChangeEntity -> DB -> DB
processChangeEntity time entity db = db'' where
  properties = changeProperties entity
  db'' = foldl (\db' x -> processChangeProperty time (changeTable entity) (changeId entity) x db') db properties

processChangeProperty :: Text -> Text -> Text -> ChangeProperty -> DB -> DB
processChangeProperty date table uuid (ChangeProperty property op value) db =
  case M.lookup key db of
    Nothing ->
      case op of
        "=" -> M.insert key (String value) db
        "+" -> M.insert key (Array $ V.fromList [String value]) db
        "-" -> db
    Just v ->
      case op of
        "=" -> M.insert key (String value) db
        "+" -> M.insert key v' db where
          v' = case v of
            Array a -> Array $ V.snoc a (String value)
            _ -> Array $ V.fromList [v, (String value)]
        "-" -> M.delete key db
  where
    key = (table, uuid, property)
