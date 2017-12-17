module Day17 where

spinlock1 :: Int -> Int
spinlock1 steps = buffer !! succ pos
  where
    (buffer, pos) = foldl op ([0], 0) [1..2017]
    op (buffer, pos) n = (buffer', pos')
      where
        pos' = succ $ (pos + steps) `mod` n
        beforeElems = take pos' buffer
        afterElems = drop pos' buffer
        buffer' = beforeElems ++ [n] ++ afterElems

spinlock2 :: Int -> Int
spinlock2 steps = result
  where
    (_, _, result) = foldl op (0, 0, -1) [1..50000000]
    op (pos, zeroPos, result) n = (pos', zeroPos', result')
      where
        pos' = succ $ (pos + steps) `mod` n
        zeroPos' = if pos' <= zeroPos then succ zeroPos else zeroPos
        result' = if pos' == succ zeroPos then n else result
