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

module Utils
( Validation
, concatEithers1
, concatEithersN
, maybeToEither
, maybeToValidation
, strip
) where

import qualified Data.Text as T

type Validation a = Either [String] a

-- Extract errors
concatEithers1 :: [Either a b] -> Either [a] [b]
concatEithers1 xs = concatEithers1' xs [] []
concatEithers1' :: [Either a b] -> [a] -> [b] -> Either [a] [b]
concatEithers1' [] [] bs = Right (reverse bs)
concatEithers1' [] as _ = Left (reverse as)
concatEithers1' ((Left a):xs) as bs = concatEithers1' xs (a:as) bs
concatEithers1' ((Right b):xs) as bs = concatEithers1' xs as (b:bs)

-- Extract errors
concatEithersN :: [Either [a] b] -> Either [a] [b]
concatEithersN xs = concatEithersN' xs [] []
concatEithersN' :: [Either [a] b] -> [a] -> [b] -> Either [a] [b]
concatEithersN' [] [] bs = Right (reverse bs)
concatEithersN' [] as _ = Left (reverse as)
concatEithersN' ((Left as'):xs) as bs = concatEithersN' xs (as'++as) bs
concatEithersN' ((Right b):xs) as bs = concatEithersN' xs as (b:bs)

maybeToEither = flip maybe Right . Left

maybeToValidation :: Maybe a -> [String] -> Validation a
maybeToValidation m msgs = case m of
  Just x -> Right x
  Nothing -> Left msgs

strip :: String -> String
strip s = (T.unpack . T.strip . T.pack) s
