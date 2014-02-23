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

module OnTopOfThings.Parsers.ViewParser
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


data ViewElement
  = ViewElement_Value String [String]
  | ViewElement_And [ViewElement_Value]
  | ViewElement_Or [ViewElement_Value]
  deriving (Show)

parseView :: String -> Validation [ViewElement]
parseView s = case parse pmain "View" s of
  Left msg -> Left $ ("Couldn't parse view specification: "++s) : (show msg) : []
  Right l -> Right l

pmain :: Parser [ViewElement]
pmain = many (pcall <|> pstring)

pone :: Parser ViewElement
pone = (pvalue <|> pand <|> por)

pvalue :: Parser ViewElement
pvalue = do
  name <- identifier
  char '='
  value <- many1 (noneOf " ")
  return (ViewElement_Value name [value])

pand :: Parser ViewElement
pand = do
  braces pand'
  where
    pand' = do
      string "and"
      many1 pone

pcall :: Parser ViewElement
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

pstring :: Parser ViewElement
pstring = do
  s <- many1 (noneOf "$")
  return (ItemFormatElement_String s)

-- The lexer
lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
identifier  = P.identifier lexer
stringLiteral = P.stringLiteral lexer
