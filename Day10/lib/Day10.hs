module Day10 where

processList :: Int -> [Int] -> Int
processList size xs =
  a * b
  where
    a:b:_ = loop size circle xs 0 0
    circle = take size [0..]

loop :: Int -> [Int] -> [Int] -> Int -> Int -> [Int]
loop size circle xs curr skip =
  case xs of
    []   -> circle
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
