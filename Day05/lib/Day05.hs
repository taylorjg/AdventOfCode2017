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
  case Map.lookup curr m of
    Nothing -> steps
    Just jump ->
      loop m' curr' steps'
      where
        curr' = curr + jump
        jump' = succ jump
        steps' = succ steps
        m' = Map.insert curr jump' m
