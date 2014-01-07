{-
Copyright (C) 2014  Ellis Whitehead

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

module OnTopOfThings.Parsers.CardParser
--( parseNumberList
--) where
where

import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Utils


parseNumberList :: String -> Validation [String]
parseNumberList s = case parse pmain "NumberList" s of
  Left _ -> Left ["Couldn't parse ID list: "++s]
  Right l -> Right l

pmain :: Parser [String]
pmain = do
  ranges <- prange `sepBy` (char ',')
  eof
  return $ concat ranges

pnum = do
  s <- many1 digit
  return $ (read s :: Int)

_ALPHANUM :: String
_ALPHANUM = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

puuid = try $ do
  part1 <- count 8 alphaNum
  char '-'
  part2 <- count 4 alphaNum
  char '-'
  part3 <- count 4 alphaNum
  char '-'
  part4 <- count 4 alphaNum
  char '-'
  part5 <- count 12 alphaNum
  return $ [intercalate "-" [part1, part2, part3, part4, part5]]

pident = do
  a <- letter
  as <- many $ oneOf $ _ALPHANUM ++ "/-_"
  return $ ([a:as] :: [String])

prange = puuid <|> pident <|> prange2 <|> prange

prange2 = do
  l <- pnum `sepBy` (char '-')
  let min = minimum l
  let max = maximum l
  return $ map show [min..max]

prange1 = do
  n <- pnum
  return $ map show [n]
