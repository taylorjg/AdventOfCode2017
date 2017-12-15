module Day15 where

import           Data.Bits
import           Data.Int  (Int64)
import           Data.List (iterate)

judge1 :: Int64 -> Int64 -> Int64
judge1 startA startB = run startA startB predicate predicate 40000000
  where
    predicate _ = True

judge2 :: Int64 -> Int64 -> Int64
judge2 startA startB = run startA startB predicateA predicateB 5000000
  where
    predicateA a = a `mod` 4 == 0
    predicateB b = b `mod` 8 == 0

run :: Int64 -> Int64 -> (Int64 -> Bool) -> (Int64 -> Bool) -> Int -> Int64
run startA startB pA pB n =
  count
  where
    count = foldl op 0 zs
    op acc (a, b) = acc + cmp
      where
        cmp = if c == d then 1 else 0
        c = a .&. 0xffff
        d = b .&. 0xffff
    xs = take n $ filter pA $ generate startA 16807
    ys = take n $ filter pB $ generate startB 48271
    zs = zip xs ys

generate :: Int64 -> Int64 -> [Int64]
generate start factor =
  drop 1 $ iterate f start
  where
    f x = (x * factor) `rem` 2147483647
