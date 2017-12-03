module Day02 where

checksum :: [[Int]] -> Int
checksum xss = sum diffs
  where
    diffs = map calcDiff pairs
    pairs = map findMinMax xss
    calcDiff (max, min) = max - min
    findMinMax xs = (maximum xs, minimum xs)
