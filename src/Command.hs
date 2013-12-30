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

readCommandRecord :: String -> IO (Either String [CommandRecord])
readCommandRecord filename = do
  content <- B.readFile filename
  return $ processContent content
  where
    processContent content = result where
      result0 = eitherDecode content :: Either String Value
      result = case result0 of
        Left msg -> Left msg
        Right value@(Object _) -> case fromJSON value :: Result CommandRecord of
          Error msg -> Left msg
          Success record -> Right [record]
        Right value@(Array _) -> case fromJSON value :: Result [CommandRecord] of
          Error msg -> Left msg
          Success records -> Right records

loadCommandRecords :: IO (Either String [CommandRecord])
loadCommandRecords = do
  --let files = ["testdata/command001.json", "testdata/command002.json"]
  files' <- getDirectoryContents "testdata"
  let files = filter (\f -> takeExtension f == ".json") files'
  --files <- FF.find (return False) (FF.extension `FF.==?` ".json") "testdata"
  records <- mapM (\f -> readCommandRecord (joinPath ["testdata", f])) files
  return $ foldl fn (Right [] :: Either String [CommandRecord]) records where
    fn :: Either String [CommandRecord] -> Either String [CommandRecord] -> Either String [CommandRecord]
    fn (Right acc) (Right records) = Right $ acc ++ records
    fn (Left msg) _ = Left msg
    fn _ (Left msg) = Left msg

saveCommandRecord :: CommandRecord -> FilePath -> IO ()
saveCommandRecord record chguuid = do
  let filepath = joinPath ["testdata", chguuid ++ ".json"]
  let s  = BL.unpack $ encode record
  putStrLn s
  writeFile filepath s
