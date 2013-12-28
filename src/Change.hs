{-# LANGUAGE OverloadedStrings #-}

module Change
( ChangeRecord(..)
, ChangeEntity(..)
, ChangeProperty(..)
, DB
, readChangeRecord
, loadDB
, saveChangeRecord
, testChangeRecord
) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, concat)
import Data.Time.Clock (UTCTime)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, joinPath)
--import qualified System.FilePath.Find as FF
import Text.Regex (mkRegex, matchRegexAll)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Vector as V


data ChangeRecord = ChangeRecord
  { changeFormat :: Int
  , changeTime :: !UTCTime
  , changeEntities :: [ChangeEntity]
  } deriving (Show)

data ChangeEntity = ChangeEntity
  { changeTable :: !Text
  , changeId :: !Text
  , changeProperties :: [ChangeProperty]
  } deriving (Show)

data ChangeProperty = ChangeProperty !Text !Text !Text
  deriving (Show)

instance ToJSON ChangeRecord where
  toJSON (ChangeRecord format time entities) = object (l1 ++ l2) where
    l1 = ["format" .= format, "time" .= time]
    l2 = case entities of
      entity : [] -> getEntityPropertyPairs entity
      _ -> ["entities" .= entities]

instance FromJSON ChangeRecord where
  parseJSON (Object v) = ChangeRecord <$> format <*>  time <*> list where
    format = v .: "format"
    time = v .: "time"
    list = case HM.lookup "list" v of
      Just a -> parseJSON a
      Nothing -> ((parseJSON (Object v)) :: Parser ChangeEntity) >>= (\x -> return [x])
  parseJSON _ = empty

instance ToJSON ChangeEntity where
  toJSON entity = object $ getEntityPropertyPairs entity

instance FromJSON ChangeEntity where
  parseJSON (Object v) = ChangeEntity <$> table <*> id <*> properties where
    table = v .:? "table" .!= "item"
    id = v .: "id"
    properties = v .: "properties"
  parseJSON _ = empty

instance ToJSON ChangeProperty where
  toJSON (ChangeProperty name op value) = String (Data.Text.concat [name, op, value])

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

--getEntityPropertyPairs :: ChangeEntity -> [(String, Value)]
getEntityPropertyPairs (ChangeEntity table id properties) =
  catMaybes [if table == "table" then Just ("table" .= table) else Nothing, Just ("id" .= id), Just ("properties" .= properties)]

readChangeRecord :: String -> IO (Either String ChangeRecord)
readChangeRecord filename = do
  (eitherDecode <$> B.readFile filename) :: IO (Either String ChangeRecord)

testChangeRecord :: IO ()
testChangeRecord = do
  x <- loadDB
  case x of
    Right db -> do
      let l = M.toList db
      mapM_ (\(k, v) -> putStrLn $ (show k) ++ (show v)) l
    Left msg -> do
      putStrLn msg

loadDB :: IO (Either String DB)
loadDB = do
  --let files = ["testdata/change001.json", "testdata/change002.json"]
  files' <- getDirectoryContents "testdata"
  let files = filter (\f -> takeExtension f == ".json") files'
  --files <- FF.find (return False) (FF.extension `FF.==?` ".json") "testdata"
  let db0 = M.empty :: DB
  records <- mapM (\f -> readChangeRecord (joinPath ["testdata", f])) files
  return $ foldl fn (Right db0) records where
    fn (Right db) (Right record) = Right $ processChangeRecord record db
    fn (Left msg) _ = Left msg
    fn _ (Left msg) = Left msg

saveChangeRecord :: ChangeRecord -> IO ()
saveChangeRecord record = do
  putStrLn $ BL.unpack $ encode record

type DB = M.Map (Text, Text, Text) Value

processChangeRecord :: ChangeRecord -> DB -> DB
processChangeRecord record db = db'' where
  entities = changeEntities record
  db'' = foldl (\db' entity -> processChangeEntity (changeTime record) entity db') db entities

processChangeEntity :: UTCTime -> ChangeEntity -> DB -> DB
processChangeEntity time entity db = db'' where
  properties = changeProperties entity
  db'' = foldl (\db' x -> processChangeProperty time (changeTable entity) (changeId entity) x db') db properties

processChangeProperty :: UTCTime -> Text -> Text -> ChangeProperty -> DB -> DB
processChangeProperty time table uuid (ChangeProperty property op value) db =
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
