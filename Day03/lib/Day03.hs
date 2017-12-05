module Day03 (spiralDistance) where

data Coords = Coords { x :: Int, y :: Int }

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
