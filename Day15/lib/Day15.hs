module Day15 where

import           Data.Bits
import           Data.Int  (Int64)
import           Data.List (iterate)

judge :: Int64 -> Int64 -> Int64
judge startA startB = run startA startB 40000000

run :: Int64 -> Int64 -> Int -> Int64
run startA startB n =
  count
  where
    count = foldl op 0 zs
    op acc (a, b) = acc + cmp
      where
        cmp = if c == d then 1 else 0
        c = a .&. 0xffff
        d = b .&. 0xffff
    xs = take n $ generate startA 16807
    ys = take n $ generate startB 48271
    zs = zip xs ys

generate :: Int64 -> Int64 -> [Int64]
generate start factor =
  drop 1 $ iterate f start
  where
    f x = (x * factor) `rem` 2147483647
