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
  | ViewElement_And [ViewElement]
  | ViewElement_Or [ViewElement]
  deriving (Show)

parseView :: String -> Validation ViewElement
parseView s = case parse pone "View" s of
  Left msg -> Left $ ("Couldn't parse view specification: "++s) : (show msg) : []
  Right l -> Right l

pone :: Parser ViewElement
pone = do
  x <- (plist <|> pvalue)
  return x

pvalue :: Parser ViewElement
pvalue = do
  name <- identifier
  char '='
  values <- commaSep1 (many $ noneOf " ),")
  return (ViewElement_Value name values)

plist :: Parser ViewElement
plist = do
  parens plist'
  where
    plist' = pand' <|> por'
    pand' = do
      string "and"
      spaces
      elems <- sepBy pone (many1 space)
      return (ViewElement_And elems)
    por' = do
      string "or"
      spaces
      elems <- many pone
      return (ViewElement_Or elems)

-- The lexer
lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
identifier  = P.identifier lexer
stringLiteral = P.stringLiteral lexer
commaSep1   = P.commaSep1 lexer
semiSep     = P.semiSep lexer
