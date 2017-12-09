module Day09 where

-- <>, empty garbage.
-- <random characters>, garbage containing random characters.
-- <<<<>, because the extra < are ignored.
-- <{!>}>, because the first > is canceled.
-- <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
-- <!!!>>, because the second ! and the first > are canceled.
-- <{o"i!a,<{i<a>, which ends at the first >.
parseGarbage :: String -> Int
parseGarbage s =
  loop s False 0
  where
    loop s escape count =
      case s of
        []     -> -1
        '>':_  | not escape -> succ count
        '!':tl | not escape -> loop tl True (succ count)
        _:tl   -> loop tl False (succ count)

-- {}, 1 group.
-- {{{}}}, 3 groups.
-- {{},{}}, also 3 groups.
-- {{{},{},{{}}}}, 6 groups.
-- {<{},{},{{}}>}, 1 group (which itself contains garbage).
-- {<a>,<a>,<a>,<a>}, 1 group.
-- {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
-- {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
parseGroups :: String -> Int
parseGroups s = fst $ loop s 0 0 []

loop s gCount gDepth gDepths =
  case s of
    []     -> (-1, [])
    '{':tl ->
      loop tl (succ gCount) gDepth' (gDepth':gDepths)
      where
        gDepth' = succ gDepth
    '}':tl | gDepth == 1 -> (gCount, gDepths)
    '}':tl -> loop tl gCount (pred gDepth) gDepths
    '<':tl ->
      loop (drop garbageLength s) gCount gDepth gDepths
      where
        garbageLength = parseGarbage s
    ',':tl -> loop tl gCount gDepth gDepths

-- {}, score of 1.
-- {{{}}}, score of 1 + 2 + 3 = 6.
-- {{},{}}, score of 1 + 2 + 2 = 5.
-- {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
-- {<a>,<a>,<a>,<a>}, score of 1.
-- {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
-- {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
-- {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
scoreGroups :: String -> Int
scoreGroups s = sum $ snd $ loop s 0 0 []
