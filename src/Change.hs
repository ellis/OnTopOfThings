{-# LANGUAGE OverloadedStrings #-}

module Change
( ChangeFile(..)
, ChangeSet(..)
, ChangeBlock(..)
, ChangeProperty(..)
, DB
, readChangeFile
, loadDB
, testChangeFile
) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Text (Text)

data ChangeFile = ChangeFile
  { format :: Int
  , changeSet :: [ChangeSet]
  } deriving (Show)

data ChangeSet = ChangeSet
  { date :: !Text
  , changeBlock :: [ChangeBlock]
  } deriving (Show)

data ChangeBlock = ChangeBlock
  { table :: !Text
  , uuid :: !Text
  , changeList :: [ChangeProperty]
  } deriving (Show)

data ChangeProperty = ChangeProperty !Text !Text !Text
  deriving (Show)

instance FromJSON ChangeFile where
  parseJSON (Object v) = ChangeFile <$> v .: "format" <*> v .: "changes"
  parseJSON _ = empty

instance FromJSON ChangeSet where
  parseJSON (Array v) = ChangeSet <$> date <*> changeBlock where
    date = parseJSON (v V.! 0)
    changeBlock = parseJSON $ Array (V.drop 1 v)
  parseJSON _ = empty

instance FromJSON ChangeBlock where
  parseJSON (Array v) = ChangeBlock <$> table <*> uuid <*> changeList where
    table = parseJSON (v V.! 0)
    uuid = parseJSON (v V.! 1)
    changeList = parseJSON $ Array (V.drop 2 v)
  parseJSON _ = empty

instance FromJSON ChangeProperty where
  parseJSON (Array v) = ChangeProperty <$> name <*> op <*> value where
    name = parseJSON (v V.! 0)
    op = parseJSON (v V.! 1)
    value = parseJSON (v V.! 2)
  parseJSON _ = empty

readChangeFile :: String -> IO (Either String ChangeFile)
readChangeFile filename = do
  (eitherDecode <$> B.readFile filename) :: IO (Either String ChangeFile)

testChangeFile :: IO ()
testChangeFile = do
  d <- readChangeFile "testdata/change001.json"
  putStrLn $ show d
  case d of
    Right file -> do
      let db0 = M.empty :: DB
      let db = processChangeFile file db0
      let l = M.toList db
      mapM_ (\(k, v) -> putStrLn $ (show k) ++ (show v)) l

loadDB :: IO DB
loadDB = do
  d <- readChangeFile "testdata/change001.json"
  case d of
    Right file -> do
      let db0 = M.empty :: DB
      return $ processChangeFile file db0

type DB = M.Map (Text, Text, Text) Value

processChangeFile :: ChangeFile -> DB -> DB
processChangeFile file db = db'' where
  l = changeSet file
  db'' = foldl (\db' x -> processChangeSet x db') db l

processChangeSet :: ChangeSet -> DB -> DB
processChangeSet set db = db'' where
  l = changeBlock set
  db'' = foldl (\db' x -> processChangeBlock (date set) x db') db l

processChangeBlock :: Text -> ChangeBlock -> DB -> DB
processChangeBlock date block db = db'' where
  l = changeList block
  db'' = foldl (\db' x -> processChangeProperty date (table block) (uuid block) x db') db l

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
