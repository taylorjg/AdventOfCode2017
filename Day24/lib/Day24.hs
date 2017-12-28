module Day24 where

import           Data.List       ((\\))
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
validBridges components = go startingBridges
  where
    startingBridges = map (:[]) startingComponents
    startingComponents = filter (canConnect 0) components
    go bridges =
      case nextBridges of
        [] -> bridges
        _  -> go nextBridges
      where
        nextBridges = concatMap advance bridges
        advance bridge = map (:bridge) matches
          where
            rest = components \\ bridge
            port = lastUnmatchedPort bridge
            matches = filter (canConnect port) rest
    canConnect port component = a component == port || b component == port
    lastUnmatchedPort bridge =
      case bridge of
        Component a b:Component c d:_ | a == c || a == d -> b
        Component a b:Component c d:_ | b == c || b == d -> a
        [Component a b]               | a == 0               -> b
        [Component a b]               | b == 0               -> a
        []                            -> 0

computePart1 :: [Component] -> Int
computePart1 components =
  trace ("bridges: " ++ show bridges) $
  maximum $ map bridgeStrength bridges
  where
    bridges = validBridges components

computePart2 :: [Component] -> Int
computePart2 components =
  trace ("bridges: " ++ show bridges) $
  maximum $ map bridgeStrength longestBridges
  where
    bridges = validBridges components
    maxLength = maximum $ map length bridges
    longestBridges = filter ((== maxLength) . length) bridges
