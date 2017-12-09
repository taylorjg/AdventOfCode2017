module Day09 where

-- <>, empty garbage.
-- <random characters>, garbage containing random characters.
-- <<<<>, because the extra < are ignored.
-- <{!>}>, because the first > is canceled.
-- <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
-- <!!!>>, because the second ! and the first > are canceled.
-- <{o"i!a,<{i<a>, which ends at the first >.
parseGarbage :: String -> Int
parseGarbage s = fst $ loop1 s False 0 0

-- <>, 0 characters.
-- <random characters>, 17 characters.
-- <<<<>, 3 characters.
-- <{!>}>, 2 characters.
-- <!!>, 0 characters.
-- <!!!>>, 0 characters.
-- <{o"i!a,<{i<a>, 10 characters.
lengthOfGarbageContent :: String -> Int
lengthOfGarbageContent s = snd $ loop1 s False 0 0

loop1 s escape count contentLength =
  case s of
    []     -> (-1, -1)
    '<':tl | count > 0 -> loop1 tl False (succ count) (succ contentLength)
    '<':tl -> loop1 tl False (succ count) contentLength
    '>':_  | not escape -> (succ count, contentLength)
    '!':tl | not escape -> loop1 tl True (succ count) contentLength
    _:tl   | escape  -> loop1 tl False (succ count) contentLength
    _:tl   -> loop1 tl False (succ count) (succ contentLength)

-- {}, 1 group.
-- {{{}}}, 3 groups.
-- {{},{}}, also 3 groups.
-- {{{},{},{{}}}}, 6 groups.
-- {<{},{},{{}}>}, 1 group (which itself contains garbage).
-- {<a>,<a>,<a>,<a>}, 1 group.
-- {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
-- {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
parseGroups :: String -> Int
parseGroups s = length $ loop2 s 0 []

-- {}, score of 1.
-- {{{}}}, score of 1 + 2 + 3 = 6.
-- {{},{}}, score of 1 + 2 + 2 = 5.
-- {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
-- {<a>,<a>,<a>,<a>}, score of 1.
-- {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
-- {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
-- {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
scoreGroups :: String -> Int
scoreGroups s = sum $ loop2 s 0 []

loop2 s gDepth gDepths =
  case s of
    []     -> gDepths
    '{':tl ->
      loop2 tl gDepth' (gDepth':gDepths)
      where
        gDepth' = succ gDepth
    '}':tl | gDepth == 1 -> gDepths
    '}':tl -> loop2 tl (pred gDepth) gDepths
    '<':tl ->
      loop2 (drop garbageLength s) gDepth gDepths
      where
        garbageLength = parseGarbage s
    ',':tl -> loop2 tl gDepth gDepths
