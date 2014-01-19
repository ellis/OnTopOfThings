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

module OnTopOfThings.Data.Time
where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (mplus, msum)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.ISO8601
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)

import Utils

data Time = Time
  { otimeDay :: Day
  , otimeHour :: Maybe Int
  , otimeMinute :: Maybe Int
  , otimeSecond :: Maybe Int
  , otimeZone :: Maybe TimeZone
  }

instance Show Time where
  show time = formatTime' time

instance Eq Time where
  tm1 == tm2 =
    compare tm1 tm2 == EQ

instance Ord Time where
  compare (Time day1 hour1_ minute1_ second1_ tz1_) (Time day2 hour2_ minute2_ second2_ tz2_) =
    case compare day1 day2 of
      EQ -> compare l1 l2 where
        -- TODO: get information from the timezone too!
        l1 = [fromMaybe 0 hour1_, fromMaybe 0 minute1_, fromMaybe 0 second1_]
        l2 = [fromMaybe 0 hour2_, fromMaybe 0 minute2_, fromMaybe 0 second2_]
      x -> x

-- For some time handling notes, see: <http://tab.snarc.org/posts/haskell/2011-12-16-date-in-haskell.html>

parseTime' :: TimeZone -> String -> Validation Time
parseTime' tz s = msum x `maybeToValidation` ["Could not parse time: "++s] where
  l = [("%FT%T%QZ", 3, True), ("%Y-%m-%d", 0, False), ("%Y-%m-%dT%H:%M", 2, False), ("%Y-%m-%dT%H:%M:%S", 3, False), ("%Y-%m-%dT%H:%M%Z", 2, True), ("%Y-%m-%dT%H:%M:%S%Z", 3, True)]
  x = map step l
  step :: (String, Int, Bool) -> Maybe Time
  step (format, n, hasTz) =
    case (parseTime defaultTimeLocale format s :: Maybe ZonedTime) of
      Nothing -> Nothing
      Just zoned -> Just $ zonedToTimeN zoned n hasTz

--parseTimeDate :: String -> Maybe Day
--parseTimeDate s =
--  (fn "%Y-%m-%d") `mplus` (fn "%m/%d/%Y") `mplus` (fn "%d.%m.%Y")
--  where
--    fn :: String -> Maybe Day
--    fn format = parseTime defaultTimeLocale format s
--
--parseTimeTime :: String -> Maybe TimeOfDay
--parseTimeTime s =
--  (fn "%H:%M") `mplus` (fn "%H:%M:%S") `mplus` (fn "%Hh")
--  where
--    fn format = parseTime defaultTimeLocale format s

formatTime' :: Time -> String
formatTime' (Time day hour minute second zone) = d_s ++ t_s ++ z_s where
  d_s = formatTime defaultTimeLocale "%Y-%m-%d" day
  t_s = case (hour, minute, second) of
    (Just h, Nothing, _) -> "T" ++ (formatTime defaultTimeLocale "%H:%M" difftime) where
      difftime = timeToTimeOfDay (fromIntegral (h*60*60) :: DiffTime)
    (Just h, Just m, Nothing) -> "T" ++ (formatTime defaultTimeLocale "%H:%M" difftime) where
      difftime = timeToTimeOfDay (fromIntegral ((h*60 + m)*60) :: DiffTime)
    (Just h, Just m, Just s) -> "T" ++ (formatTime defaultTimeLocale "%H:%M:%S" difftime) where
      difftime = timeToTimeOfDay (fromIntegral ((h*60 + m)*60 + s) :: DiffTime)
    _ -> ""
  z_s = case zone of
    Just tz -> formatTime defaultTimeLocale "%Z" tz
    _ -> ""

zonedToTime :: ZonedTime -> Time
zonedToTime zoned = Time day (Just (todHour tod)) (Just (todMin tod)) (Just (round $ todSec tod)) (Just tz) where
  local = zonedTimeToLocalTime zoned
  day = localDay local
  tod = localTimeOfDay local
  tz = zonedTimeZone zoned

zonedToTimeN :: ZonedTime -> Int -> Bool -> Time
zonedToTimeN zoned n hasTz = Time day (ifn 1 (todHour tod)) (ifn 2 (todMin tod)) (ifn 3 (round $ todSec tod)) (if hasTz then Just tz else Nothing) where
  local = zonedTimeToLocalTime zoned
  day = localDay local
  tod = localTimeOfDay local
  tz = zonedTimeZone zoned
  ifn n' _ | n' > n = Nothing
  ifn n' x = Just x

utcToTime :: UTCTime -> TimeZone -> Time
utcToTime utc tz = zonedToTime (utcToZonedTime tz utc)
