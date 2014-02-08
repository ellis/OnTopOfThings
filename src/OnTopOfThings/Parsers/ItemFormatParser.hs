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

module OnTopOfThings.Parsers.ItemFormatParser
--( parseNumberList
--) where
where

import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Utils


data ItemFormatElement
  = ItemFormatElement_String String
  | ItemFormatElement_Call
    { callName :: String
    , callMissing :: String
    , callPrefix :: String
    , callInfix :: String
    , callSuffix :: String
    }
  deriving (Show)

parseItemFormat :: String -> Validation [ItemFormatElement]
parseItemFormat s = case parse pmain "ItemFormat" s of
  Left _ -> Left ["Couldn't parse item format string: "++s]
  Right l -> Right l

pmain :: Parser [ItemFormatElement]
pmain = many (pcall <|> pchar)

pcall :: Parser ItemFormatElement
pcall = do
  char '$'
  name <- braces identifier
  return (ItemFormatElement_Call name "" "" "" "")

pchar :: Parser ItemFormatElement
pchar = do
  s <- many1 (noneOf "$")
  return (ItemFormatElement_String s)

_ALPHANUM :: String
_ALPHANUM = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- The lexer
lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
