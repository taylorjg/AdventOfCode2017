module Day01 where

import           Data.Map (fromList, (!))

captcha1 :: String -> Int
captcha1 [] = 0
captcha1 xs = sum ns
  where
    pairs = zip xs (tail xs ++ take 1 xs)
    matchingPairs = filter matchingPair pairs
    ns = map fstToInt matchingPairs

captcha2 :: String -> Int
captcha2 [] = 0
captcha2 xs = sum ns
  where
    pairs = map (\(idx, x) -> (x, m ! ((idx + n) `mod` len))) zipped
    matchingPairs = filter matchingPair pairs
    ns = map fstToInt matchingPairs
    zipped = zip [0..] xs
    m = fromList zipped
    len = length xs
    n = len `div` 2

matchingPair :: Eq a => (a, a) -> Bool
matchingPair (a, b) = a == b

fstToInt :: (Char, a) -> Int
fstToInt (a, _) = read [a]
