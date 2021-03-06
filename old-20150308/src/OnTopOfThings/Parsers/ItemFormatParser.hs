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
  Left msg -> Left $ ("Couldn't parse item format string: "++s) : (show msg) : []
  Right l -> Right l

pmain :: Parser [ItemFormatElement]
pmain = many (pcall <|> pstring)

pcall :: Parser ItemFormatElement
pcall = do
  char '$'
  braces pcall'
  where
    pcall' = do
      name <- identifier
      missing <- parg
      prefix <- parg
      infix_ <- parg
      suffix <- parg
      return (ItemFormatElement_Call name missing prefix infix_ suffix)

parg :: Parser String
parg = do
  option "" (do
    stringLiteral)

pstring :: Parser ItemFormatElement
pstring = do
  s <- many1 (noneOf "$")
  return (ItemFormatElement_String s)

-- The lexer
lexer       = P.makeTokenParser haskellDef
braces      = P.braces lexer
identifier  = P.identifier lexer
stringLiteral = P.stringLiteral lexer
