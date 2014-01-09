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

module OnTopOfThings.Data.Patch
--( parseNumberList
--) where
where

import Data.Maybe
import Data.Time (UTCTime, getCurrentTime)

import qualified Data.Map as M
import qualified Data.Set as Set

import Utils


data Patch = Patch
  { patchFormat :: String
  , patchTime :: UTCTime
  , patchUser :: String
  , patchUuid :: String
  , patchUuidParent :: Maybe String
  , patchHunks :: [PatchHunk]
  } deriving (Show)

data PatchHunk = PatchHunk
  { patchHunkUuids :: [String]
  , patchHunkDiffs :: [Diff]
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

data DiffMaps = DiffMaps
  { diffMapsNull :: Set.Set String
  , diffMapsEqual :: M.Map String String
  , diffMapsAdd :: M.Map String [String]
  , diffMapsUnset :: Set.Set String
  , diffMapsRemove :: M.Map String [String]
  } deriving (Show)

diffsToMaps :: [Diff] -> DiffMaps
diffsToMaps diffs = step diffs (DiffMaps Set.empty M.empty M.empty Set.empty M.empty) where
  step :: [Diff] -> DiffMaps -> DiffMaps
  step [] m = m
  step (diff:rest) m = step rest m' where
    m' = case diff of
      DiffNull name -> m { diffMapsNull = Set.insert name (diffMapsNull m) }
      DiffEqual name value -> m { diffMapsEqual = M.insert name value (diffMapsEqual m) }
      DiffAdd name value -> m { diffMapsAdd = M.insert name
        (fromMaybe [] (M.lookup name (diffMapsAdd m)) ++ [value]) (diffMapsAdd m) }
      DiffUnset name -> m { diffMapsUnset = Set.insert name (diffMapsUnset m) }
      DiffRemove name value -> m { diffMapsRemove = M.insert name
        (fromMaybe [] (M.lookup name (diffMapsRemove m)) ++ [value]) (diffMapsRemove m) }

--data ItemUpdate = ItemUpdate
--  { itemUpdateType :: Maybe String
--  , itemUpdateStatus :: Maybe String
--  , itemUpdateParent :: Maybe String
--  , itemUpdateName :: Maybe String
--  , itemUpdateTitle :: Maybe String
--  , itemUpdateStage :: Maybe String
--  , itemUpdateClosed :: Maybe UTCTime
--  , itemUpdateStart :: Maybe UTCTime
--  , itemUpdateEnd :: Maybe UTCTime
--  , itemUpdateDue :: Maybe UTCTime
--  , itemUpdateReview :: Maybe UTCTime
--  , itemUpdateIndex :: Maybe Int
--  } deriving (Show)
