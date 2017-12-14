module Day14 where

import           Data.List (nub)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Day10

squaresUsed :: String -> Int
squaresUsed = length . filter (== '1') . mkGrid

countRegions :: String -> Int
countRegions s = rids
  where
    (_, rids) = foldl op seed positions
    seed = (Map.empty, 0)
    range = [0..127]
    positions = [y * 128 + x | y <- range, x <- range]
    op acc @ (m, rid) pos = if isUsed pos
      then case Map.lookup pos m of
        Just _  -> acc
        Nothing -> (m', rid')
          where
            ns = deepNeighbours [pos] [pos]
            m2 = Map.fromList $ map (flip (,) rid') ns
            m' = Map.union m m2
            rid' = succ rid
      else acc
    deepNeighbours ps nsCurr =
      case nsNext of
        [] -> nsCurr
        _  -> deepNeighbours nsNext (nsCurr ++ nsNext)
      where
        nsNext =
          filter isUsed .
          filter inRange .
          filter notSeen .
          nub $
          concatMap immediateNeighbours ps
        notSeen p = p `notElem` nsCurr
        inRange p = p >= 0 && p < 128 * 128
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
