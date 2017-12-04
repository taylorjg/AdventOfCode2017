module Day02 where

import           Data.List (find)

checksum1 :: [[Int]] -> Int
checksum1 xss = sum diffs
  where
    diffs = map calcDiff pairs
    pairs = map findMinMax xss
    calcDiff (max, min) = max - min
    findMinMax xs = (maximum xs, minimum xs)

checksum2 :: [[Int]] -> Int
checksum2 xss = sum divisions
  where
    divisions = map findDivisons xss
    findDivisons [] = 1
    findDivisons (a:tl) =
      case (mb1, mb2) of
        (Just b, Nothing) -> a `div` b
        (Nothing, Just b) -> b `div` a
        _                 -> findDivisons tl
      where
        mb1 = find (\b -> b /= 0 && a `rem` b == 0) tl
        mb2 = find (\b -> a /= 0 && b `rem` a == 0) tl
