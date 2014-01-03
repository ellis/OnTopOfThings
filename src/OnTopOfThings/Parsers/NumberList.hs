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

module OnTopOfThings.Parsers.NumberList
--( parseNumberList
--) where
where

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

pident = do
  a <- letter
  as <- many alphaNum
  return $ ([a:as] :: [String])

prange = pident <|> prange2 <|> prange

prange2 = do
  l <- pnum `sepBy` (char '-')
  let min = minimum l
  let max = maximum l
  return $ map show [min..max]

prange1 = do
  n <- pnum
  return $ map show [n]
