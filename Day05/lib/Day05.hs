module Day05 (escapeMaze1, escapeMaze2) where

import           Data.Map (Map)
import qualified Data.Map as Map

escapeMaze1 :: [Int] -> Int
escapeMaze1 jumps =
  loop m 0 0 fn
  where
    m = Map.fromList $ zip [0 :: Int ..] jumps
    fn = succ

escapeMaze2 :: [Int] -> Int
escapeMaze2 jumps =
  loop m 0 0 fn
  where
    m = Map.fromList $ zip [0 :: Int ..] jumps
    fn jump = case jump of
      _ | jump >= 3 -> pred jump
      _ -> succ jump

loop :: Map Int Int -> Int -> Int -> (Int -> Int) -> Int
loop m curr steps fn =
  case Map.lookup curr m of
    Nothing -> steps
    Just jump -> loop m' curr' steps' fn
      where
        curr' = curr + jump
        jump' = fn jump
        steps' = succ steps
        m' = Map.insert curr jump' m
