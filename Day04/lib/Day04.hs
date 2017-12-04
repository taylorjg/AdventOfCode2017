module Day04 where

import           Data.List (and, concatMap, notElem, permutations)
import           Data.Set  (fromList, size)

validatePassPhrase1 :: String -> Bool
validatePassPhrase1 passPhrase =
  size set == length ws
  where
    ws = words passPhrase
    set = fromList ws

validatePassPhrase2 :: String -> Bool
validatePassPhrase2 passPhrase =
  validatePassPhrase1 passPhrase && and bs
  where
    ws = words passPhrase
    bs = map isValidWord ws
    otherWords w = filter (/= w) ws
    isValidWord w = w `notElem` allAnagrams
      where
        allAnagrams = concatMap permutations $ otherWords w
