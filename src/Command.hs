{-# LANGUAGE OverloadedStrings #-}

module Command
( CommandRecord(..)
, readCommandRecord
, loadCommandRecords
, saveCommandRecord
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


data CommandRecord = CommandRecord
  { commandFormat :: Int
  , commandTime :: !UTCTime
  , commandUser :: !Text
  , commandCmd :: !Text
  , commandArgs :: [Text]
  } deriving (Show)

instance ToJSON CommandRecord where
  toJSON (CommandRecord format time user cmd args) =
    object ["format" .= format, "time" .= time, "user" .= user, "cmd" .= cmd, "args" .= args]

instance FromJSON CommandRecord where
  parseJSON (Object v) = CommandRecord <$> format <*>  time <*> user <*> cmd <*> args where
    format = v .: "format"
    time = v .: "time"
    user = v .:? "user" .!= "default"
    cmd = v .: "cmd"
    args = v .: "args"
  parseJSON _ = empty

readCommandRecord :: String -> IO (Either String CommandRecord)
readCommandRecord filename = do
  (eitherDecode <$> B.readFile filename) :: IO (Either String CommandRecord)

loadCommandRecords :: IO (Either String [CommandRecord])
loadCommandRecords = do
  --let files = ["testdata/command001.json", "testdata/command002.json"]
  files' <- getDirectoryContents "testdata"
  let files = filter (\f -> takeExtension f == ".json") files'
  --files <- FF.find (return False) (FF.extension `FF.==?` ".json") "testdata"
  records <- mapM (\f -> readCommandRecord (joinPath ["testdata", f])) files
  return $ foldl fn (Right [] :: Either String [CommandRecord]) records where
    fn :: Either String [CommandRecord] -> Either String CommandRecord -> Either String [CommandRecord]
    fn (Right acc) (Right record) = Right $ record : acc
    fn (Left msg) _ = Left msg
    fn _ (Left msg) = Left msg

saveCommandRecord :: CommandRecord -> FilePath -> IO ()
saveCommandRecord record chguuid = do
  let filepath = joinPath ["testdata", chguuid ++ ".json"]
  let s  = BL.unpack $ encode record
  putStrLn s
  writeFile filepath s
