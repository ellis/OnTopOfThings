{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data ChangeProperty = ChangeProperty String String [String]
  deriving (Show)

--instance FromJSON ChangeProperty where
--  parseJSON (Object v) =
--    ChangeProperty <$>
--    (v .: 

-- {
--  "format": 1,
--  "changes": [
--   ["2013-12-22",
--    ["item", "UUID",
--     ["title", "=", ["improve my repos"]],
--     ["tag", "+", ["repos"]],
--     ["context", "-", ["work"]]
--    ]
--   ]
--  ]
-- }

main :: IO ()
main = do
  putStrLn "Hi"
  
  d <- (eitherDecode <$> B.readFile "x.json") :: IO (Either String Value)
  putStrLn $ show d
 -- Get JSON data and decode it
 --d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 --case d of
 -- Left err -> putStrLn err
 -- Right ps -> print ps

-- test = do
--   let json = B.pack "[\"title\", \"=\", [\"improve\"]]"
--   decode json :: Maybe Value
