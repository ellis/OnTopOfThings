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

module OnTopOfThings.Data.Utils
( parseTime'
) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monoid (mappend)
import Data.Maybe (catMaybes)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.ISO8601
import System.Locale
import Text.Regex (mkRegex, matchRegexAll)

import OnTopOfThings.Data.Types

type PropertyMap = M.Map String [String]

type Time = (Day, Maybe TimeOfDay, Maybe TimeZone)

-- For some time handling notes, see: <http://tab.snarc.org/posts/haskell/2011-12-16-date-in-haskell.html>

parseTime' :: String -> Validation Time
parseTime' s = x where
  x' = case words s of
    s1:[] -> parseTimeDate s1 >>= \day -> Just (Time day Nothing Nothing)
    s1:s2:[] ->
      case (parseTimeDate s1, parseTimeTime s2) of
        (Just day, Just time) -> Just (Time day time Nothing)
  x = case x' of
    Nothing -> Left ["Could not parse time: "++s]
    Just dt -> Right dt

parseTimeDate :: String -> Maybe Day
parseTimeDate s =
  (fn "%Y-%m-%d") `mappend` (fn "%m/%d/%Y") `mappend` (fn "%d.%m.%Y")
  where
    fn format = parseTime defaultTimeLocale format s

parseTimeTime :: String -> Maybe TimeOfDay
parseTimeTime s =
  (fn "%H:%M") `mappend` (fn "%H:%M:%S") `mappend` (fn "%Hh")
  where
    fn format = parseTime defaultTimeLocale format s
