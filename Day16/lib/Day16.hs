module Day16 (
  DanceMove(..),
  parseDanceMoves,
  dance,
  wholeDance) where

import           Data.List       (elemIndex, foldl')
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)

data DanceMove =
  Spin Int |
  Exchange Int Int |
  Partner Char Char
  deriving (Eq, Show)

parseSpin :: String -> DanceMove
parseSpin s = Spin x
  where
    x = read $ drop 1 s

parseExchange :: String -> DanceMove
parseExchange s = Exchange a b
   where
    pos = fromMaybe 0 $ elemIndex '/' s
    a = read $ take (pos - 1) $ drop 1 s
    b = read $ take (length s) $ drop (pos + 1) s

parsePartner :: String -> DanceMove
parsePartner s = Partner a b
  where
    a = s !! 1
    b = s !! 3

parseDanceMove :: String -> DanceMove
parseDanceMove s = case head s of
  's' -> parseSpin s
  'x' -> parseExchange s
  'p' -> parsePartner s

parseDanceMoves :: String -> [DanceMove]
parseDanceMoves input = map parseDanceMove $ splitOn "," input

makeSpinMove :: String -> Int -> String
makeSpinMove s x = s'
  where
    len = length s
    v1 = drop (len - x) s
    v2 = take (len - x) s
    s' = v1 ++ v2

makeExchangeMove :: String -> Int -> Int -> String
makeExchangeMove s a b = s'
  where
    cha = s !! a
    chb = s !! b
    z = zip s [0..]
    s' = map (\(ch, idx) -> case idx of
      _ | idx == a -> chb
      _ | idx == b -> cha
      _ -> ch) z

makePartnerMove :: String -> Char -> Char -> String
makePartnerMove s a b = s'
  where
    idxa = fromMaybe 0 $ elemIndex a s
    idxb = fromMaybe 0 $ elemIndex b s
    z = zip s [0..]
    s' = map (\(ch, idx) -> case idx of
      _ | idx == idxa -> b
      _ | idx == idxb -> a
      _ -> ch) z

makeMove :: String -> DanceMove -> String
makeMove s m = case m of
  Spin x       -> makeSpinMove s x
  Exchange a b -> makeExchangeMove s a b
  Partner a b  -> makePartnerMove s a b

dance' :: String -> [DanceMove] -> String
dance' = foldl makeMove

dance :: Int -> [DanceMove] -> String
dance n moves = wholeDance n moves 1

-- wholeDance :: Int -> [DanceMove] -> Int -> String
-- wholeDance n moves steps =
--   foldl' op initial [1..steps]
--   where
--     initial = take n ['a'..]
--     op s _ = dance' s moves

-- wholeDance :: Int -> [DanceMove] -> Int -> String
-- wholeDance n moves steps =
--   foldr op initial [1..steps]
--   where
--     initial = take n ['a'..]
--     op _ s = dance' s moves

-- wholeDance :: Int -> [DanceMove] -> Int -> String
-- wholeDance n moves steps =
--   result
--   where
--     result = head $ drop steps $ iterate' f initial
--     initial = take n ['a'..]
--     f s = dance' s moves

wholeDance :: Int -> [DanceMove] -> Int -> String
wholeDance n moves steps =
  result
  where
    result = fred f initial steps
    initial = take n ['a'..]
    f s = dance' s moves

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

fred :: (a -> a) -> a -> Int -> a
fred _ x 0 = x
fred f x n = x `seq` fred f (f x) (pred n)
