module Day03 (spiralDistance, firstValueWrittenBiggerThan) where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)

data Coords = Coords { x :: Int, y :: Int } deriving (Eq, Ord)

-- Part 1

spiralDistance :: Int -> Int
spiralDistance n =
  manhattanDistance $ case n of
    1 -> Coords 0 0
    _ -> calcCoords n

calcCoords :: Int -> Coords
calcCoords n = Coords x y
  where
    x = halfLen - xoffset len i
    y = -halfLen + yoffset len i
    sq = ceiling . sqrt . fromIntegral $ n
    outerSquareSize = if odd sq then sq else sq + 1
    innerSquareSize = outerSquareSize - 2
    len = outerSquareSize - 1
    halfLen = len `quot` 2
    i = n - (innerSquareSize * innerSquareSize) - 1

--         5   4   3
--         6       2
--         7   8   9 <-- relative to here (1, -1)

--     17  16  15  14  13
--     18              12
--     19              11
--     20              10
--     21  22  23  24  25 <-- relative to here (2, -2)

-- 37  36  35  34  33  32  31
-- 38                      30
-- 39                      29
-- 40                      28
-- 41                      27
-- 42                      26
-- 43  44  45  46  47  48  49 <-- relative to here (3, -3)

xoffset :: Int -> Int -> Int
xoffset len i = case q of
  0 -> 0
  1 -> r + 1
  2 -> len
  3 -> len - r - 1
  where
    q = i `quot` len
    r = i `rem` len

yoffset :: Int -> Int -> Int
yoffset len i = case q of
  0 -> r + 1
  1 -> len
  2 -> len - r - 1
  3 -> 0
  where
    q = i `quot` len
    r = i `rem` len

manhattanDistance :: Coords -> Int
manhattanDistance coords = abs (x coords) + abs (y coords)

-- Part 2

firstValueWrittenBiggerThan :: Int -> Int
firstValueWrittenBiggerThan =
  loop initialTotalsMap 2
  where
    initialTotalsMap = Map.singleton (Coords 0 0) 1

loop :: Map Coords Int -> Int -> Int -> Int
loop totalsMap n threshold =
  case total of
    _ | total > threshold -> total
    _ -> loop totalsMap' n' threshold
  where
    currentCoords = calcCoords n
    neighbourCoords = generateNeighbourCoords currentCoords
    neighbourValues = mapMaybe (`Map.lookup` totalsMap) neighbourCoords
    total = sum neighbourValues
    totalsMap' = Map.insert currentCoords total totalsMap
    n' = succ n

generateNeighbourCoords :: Coords -> [Coords]
generateNeighbourCoords (Coords x y) = neighbourCoords
  where
    deltas = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0 ]
    neighbourCoords = map addDelta deltas
    addDelta (dx, dy) = Coords (x + dx) (y + dy)
