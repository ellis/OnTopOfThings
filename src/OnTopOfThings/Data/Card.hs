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

module OnTopOfThings.Data.Card
--( parseNumberList
--) where
where

import Data.Time (UTCTime, getCurrentTime)

import Utils


data Card = Card
  { cardFormat :: String
  , cardTime :: UTCTime
  , cardUser :: String
  , cardUuid :: String
  , cardUuidParent :: Maybe String
  , cardItems :: [CardItem]
  } deriving (Show)

data CardItem = CardItem
  { cardItemUuids :: [String]
  , cardItemDiffs :: [Diff]
  } deriving (Show)

data Diff
  = DiffNull String
  | DiffEqual String String
  | DiffAdd String String
  | DiffUnset String
  | DiffRemove String String
  deriving Show

diffName :: Diff -> String
diffName x = case x of
  DiffNull name -> name
  DiffEqual name _ -> name
  DiffAdd name _ -> name
  DiffUnset name -> name
  DiffRemove name _ -> name
