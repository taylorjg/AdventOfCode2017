module Day24 where

import           Data.List.Split (splitOn)
import           Debug.Trace

data Component = Component { a :: Int, b :: Int } deriving (Show, Eq, Ord)

type Bridge = [Component]

parseComponent :: String -> Component
parseComponent line = Component a b
  where
    bits = splitOn "/" line
    a = read $ head bits
    b = read $ last bits

parseInput :: String -> [Component]
parseInput input = parseComponent `fmap` ls
  where
    ls = filter (not . null) $ splitOn "\n" input

bridgeStrength :: Bridge -> Int
bridgeStrength = foldl (\acc c -> acc + a c + b c) 0

validBridges :: [Component] -> [Bridge]
validBridges components = [[Component 0 1, Component 1 2, Component 2 3], [Component 50 50]]

computePart1 :: [Component] -> Int
computePart1 components =
  maximum $ map bridgeStrength bridges
  where
    bridges = validBridges components

computePart2 :: [Component] -> Int
computePart2 components =
  maximum $ map bridgeStrength longestBridges
  where
    bridges = validBridges components
    maxLength = maximum $ map length bridges
    longestBridges = filter ((== maxLength) . length) bridges
