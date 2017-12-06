module Day06 (numRedistributions1, numRedistributions2) where

import           Data.Foldable (maximumBy)
import           Data.Map      (Map)
import qualified Data.Map      as Map

numRedistributions1 :: [Int] -> Int
numRedistributions1 banks =
  fst $ loop [banks] 0 fn
  where
    fn bankss next = next `elem` bankss

-- Out of curiosity, the debugger would also like to know the size of the loop:
-- starting from a state that has already been seen, how many block redistribution
-- cycles must be performed before that same state is seen again?

-- In the example above, 2 4 1 2 is seen again after four cycles, and so the answer
-- in that example would be 4.

numRedistributions2 :: [Int] -> (Int, [Int])
numRedistributions2 banks =
  loop [answer1] 0 fn2
  where
    answer1 = snd $ loop [banks] 0 fn1
    fn1 bankss next = next `elem` bankss
    fn2 _ next = next == answer1

loop :: [[Int]] -> Int -> ([[Int]] -> [Int] -> Bool) -> (Int, [Int])
loop bankss steps fn =
  if fn bankss next
    then (steps', next)
    else loop (next:bankss) steps' fn
  where
    prev = head bankss
    next = redistribute prev
    steps' = succ steps

redistribute :: [Int] -> [Int]
redistribute bank =
  go m' maxBlocks $ advance chosenBank
  where
    zipped = zip [0 :: Int ..] bank
    (chosenBank, maxBlocks) = maximumBy (\(_, a) (_, b) -> a `compare` b) $ reverse zipped
    len = length bank
    m = Map.fromList zipped
    m' = Map.insert chosenBank 0 m
    advance n = succ n `mod` len
    go :: Map Int Int -> Int -> Int -> [Int]
    go m numBlocks curr =
      case numBlocks of
        0 -> map (m Map.!) [0 .. len - 1]
        _ -> go m' numBlocks' curr'
        where
          m' = Map.adjust succ curr m
          numBlocks' = pred numBlocks
          curr' = advance curr
