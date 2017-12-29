module Day11 where

import           Data.Char       (toUpper)
import           Data.List.Split (splitOn)

data Direction = N | S | SE | SW | NE | NW deriving Read

data Coords = Coords { x :: Float, y :: Float }

parsePath :: String -> [Direction]
parsePath path = map read directionsUpper
  where
    directionsUpper = map (map toUpper) directions
    directions = splitOn "," path

makeMove :: Coords -> Direction -> Coords
makeMove (Coords x y) direction =
  case direction of
    N  -> Coords x (y + 1)
    S  -> Coords x (y - 1)
    SE -> Coords (x + 1) (y - 0.5)
    SW -> Coords (x - 1) (y - 0.5)
    NE -> Coords (x + 1) (y + 0.5)
    NW -> Coords (x - 1) (y + 0.5)

makeMoves :: Coords -> [Direction] -> Coords
makeMoves = foldl makeMove

shortestDistance :: Coords -> Int
shortestDistance pt = round distance
  where
    distance = if dx > dy then dx else dy + dx / 2
    dx = abs $ x pt
    dy = abs $ y pt

findFinish :: String -> Coords
findFinish path = finish
  where
    steps = parsePath path
    start = Coords 0 0
    finish = makeMoves start steps

shortestRoute :: String -> Int
shortestRoute path =  shortestDistance $ findFinish path

furthestDistance :: String -> Int
furthestDistance path = snd finalAcc
  where
    steps = parsePath path
    start = Coords 0 0
    seed = (start, 0)
    finalAcc = foldl op seed steps
    op (location, distance) step = (location', distance')
      where
        location' = makeMove location step
        distance' = max distance $ shortestDistance location'
