module Day06 (numRedistributions) where

import           Data.Foldable (maximumBy)
import           Data.Map      (Map)
import qualified Data.Map      as Map

numRedistributions :: [Int] -> Int
numRedistributions banks = loop [banks] 0

loop :: [[Int]] -> Int -> Int
loop bankss steps =
  if next `elem` bankss
    then steps'
    else loop (next:bankss) steps'
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
