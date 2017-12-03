module Day01 where

import Data.List

captcha :: String -> Int
captcha [] = 0
captcha xs = foldl (+) 0 ns
    where
        pairs = zip xs (tail xs ++ take 1 xs)
        matchingPairs = filter matchingPair pairs
        ns = map fstToInt matchingPairs
        matchingPair (a, b) = a == b
        fstToInt (a, _) = read [a] :: Int
