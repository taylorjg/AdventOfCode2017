module Day14 where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List       (nub)
import           Data.List.Split (chunksOf)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (mapMaybe)
import           Text.Printf

squaresUsed :: String -> Int
squaresUsed s =
 v4
  where
    v1 = map addSuffix [0..127]
    addSuffix n = s ++ "-" ++ show n
    v2 = map knotHash v1
    v3 = concatMap (concatMap toBinary) v2
    v4 = length $ filter (== '1') v3

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

countRegions :: String -> Int
countRegions s =
  snd v10
  where
    v10 = foldl op2 seed positions
    op2 acc @ (m, rid) pos =
      case v3 !! pos == '1' of
          False -> acc
          True ->
            case Map.lookup pos m of
              Just _  -> acc
              Nothing ->
                (m', rid')
                where
                  ns = neighbours2 [pos] [pos]
                  m2 = Map.fromList $ map (\n -> (n, rid')) ns
                  m' = Map.union m m2
                  rid' = succ rid
    neighbours2 :: [Int] -> [Int] -> [Int]
    neighbours2 ps ns =
      case nsnew5 of
        [] -> ns
        _  -> neighbours2 nsnew5 (ns ++ nsnew5)
      where
        nsnew1 = concatMap (\p ->
          case p `mod` 128 of
            0   -> [p + 1, p + 128, p - 128]
            127 -> [p - 1, p + 128, p - 128]
            _   -> [p + 1, p - 1, p + 128, p - 128]
            ) ps
        nsnew2 = nub nsnew1
        nsnew3 = filter (\n -> n >= 0 && n < 16384) nsnew2
        nsnew4 = filter (\n -> v3 !! n == '1') nsnew3
        nsnew5 = filter (`notElem` ns) nsnew4
    v1 = map addSuffix [0..127]
    addSuffix n = s ++ "-" ++ show n
    v2 = map knotHash v1
    v3 = concatMap (concatMap toBinary) v2
    range = [0..127]
    positions = [y * 128 + x | y <- range, x <- range]
    seed = (Map.empty :: Map Int Int, 0)

processList :: Int -> [Int] -> Int
processList size xs =
  a * b
  where
    (circle', _, _) = loop size circle xs 0 0
    a:b:_ = circle'
    circle = take size [0..]

loop :: Int -> [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
loop size circle xs curr skip =
  case xs of
    []   -> (circle, curr, skip)
    x:tl -> loop size circle' tl curr' skip'
      where
        skip' = succ skip `mod` size
        curr' = (curr + x + skip) `mod` size
        wrapAmount = max ((curr + x) - size) 0
        sublist1 = take (x - wrapAmount) $ drop curr circle
        sublist2 = take wrapAmount circle
        sublist = sublist1 ++ sublist2
        sublist' = reverse sublist
        circle' = if wrapAmount > 0
          then sb1 ++ v1 ++ sb2
          else v2 ++ sublist' ++ v3
        v1 = take (size - x) $ drop wrapAmount circle
        v2 = take curr circle
        v3 = drop (curr + x) circle
        sb1 = drop (x - wrapAmount) sublist'
        sb2 = take (x - wrapAmount) sublist'

knotHash :: String -> String
knotHash s =
  intsToHexString denseHash
  where
    denseHash = makeDenseHash sparseHash
    sparseHash = makeSparseHash xs
    xs = map ord s ++ [17, 31, 73, 47, 23]

makeSparseHash :: [Int] -> [Int]
makeSparseHash xs =
  result
  where
    size = 256
    (result, _, _) = foldl op seed [0..63]
    op (circle, curr, skip) _ = loop size circle xs curr skip
    seed = (take size [0..], 0, 0)

makeDenseHash :: [Int] -> [Int]
makeDenseHash = map (foldl xor 0) . chunksOf 16

intsToHexString :: [Int] -> String
intsToHexString = concatMap (printf "%02x")
