module Day10 where

import           Data.Bits       (xor)
import           Data.Char       (ord)
import           Data.List.Split (chunksOf)
import           Text.Printf

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
