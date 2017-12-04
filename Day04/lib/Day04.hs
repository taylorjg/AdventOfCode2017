module Day04 where

import           Data.Set (fromList, size)

validatePassPhrase :: String -> Bool
validatePassPhrase passPhrase =
  size set == length ws
  where
    ws = words passPhrase
    set = fromList ws
