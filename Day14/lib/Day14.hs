module Day14 where

import           Data.List  (nub)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)
import           Day10
-- import           Text.Printf

squaresUsed :: String -> Int
squaresUsed s = length $ filter (== '1') $ mkGrid s

countRegions :: String -> Int
countRegions s = rids
  where
    (_, rids) = foldl op seed positions
    seed = (Map.empty, 0)
    range = [0..127]
    positions = [y * 128 + x | y <- range, x <- range]
    op acc @ (m, rid) pos =
      if isUsed pos
        then case Map.lookup pos m of
          Just _  -> acc
          Nothing -> (m', rid')
            where
              ns = deepNeighbours [pos] [pos]
              m2 = Map.fromList $ map (\n -> (n, rid')) ns
              m' = Map.union m m2
              rid' = succ rid
        else acc
    deepNeighbours ps ns =
      case nsnew5 of
        [] -> ns
        _  -> deepNeighbours nsnew5 (ns ++ nsnew5)
      where
        nsnew1 = concatMap immediateNeighbours ps
        nsnew2 = nub nsnew1
        nsnew3 = filter (\n -> n >= 0 && n < 16384) nsnew2
        nsnew4 = filter isUsed nsnew3
        nsnew5 = filter (`notElem` ns) nsnew4
        immediateNeighbours p =
          case p `mod` 128 of
            0   -> [p + 1, p + 128, p - 128] -- left edge
            127 -> [p - 1, p + 128, p - 128] -- right edge
            _   -> [p + 1, p - 1, p + 128, p - 128] -- elsewhere
    grid = mkGrid s
    isUsed pos = grid !! pos == '1'

mkGrid :: String -> String
mkGrid s = concatMap (concatMap toBinary . knotHash . addSuffix) [0..127]
  where
    addSuffix n = s ++ "-" ++ show n

toBinary :: Char -> String
toBinary ch = case ch of
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'a' -> "1010"
  'b' -> "1011"
  'c' -> "1100"
  'd' -> "1101"
  'e' -> "1110"
  'f' -> "1111"
