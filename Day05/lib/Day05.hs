module Day05 where

import           Data.Map (Map)
import qualified Data.Map as Map

escapeMaze :: [Int] -> Int
escapeMaze jumps =
  loop m 0 0
  where
    m = Map.fromList $ zip [0 :: Int ..] jumps

loop :: Map Int Int -> Int -> Int -> Int
loop m curr steps =
  case curr' of
    _ | curr' < 0 || curr' >= Map.size m -> steps'
    _ -> loop m' curr' steps'
  where
    jump = m Map.! curr
    jump' = succ jump
    curr' = curr + jump
    steps' = succ steps
    m' = Map.insert curr jump' m
