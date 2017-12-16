module Day16 (
  DanceMove(..),
  parseDanceMoves,
  dance,
  wholeDance) where

import           Data.List       (elemIndex, partition)
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
    x = read s

parseExchange :: String -> DanceMove
parseExchange s = Exchange a b
   where
    bits = splitOn "/" s
    a = read $ head bits
    b = read $ last bits

parsePartner :: String -> DanceMove
parsePartner s = Partner a b
  where
    a = head s
    b = last s

parseDanceMove :: String -> DanceMove
parseDanceMove ('s':rest) = parseSpin rest
parseDanceMove ('x':rest) = parseExchange rest
parseDanceMove ('p':rest) = parsePartner rest

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
makePartnerMove s a b = makeExchangeMove s idxa idxb
  where
    idxa = fromMaybe 0 $ elemIndex a s
    idxb = fromMaybe 0 $ elemIndex b s

makeMove :: String -> DanceMove -> String
makeMove s m = case m of
  Spin x       -> makeSpinMove s x
  Exchange a b -> makeExchangeMove s a b
  Partner a b  -> makePartnerMove s a b

makeMoves :: String -> [DanceMove] -> String
makeMoves = foldl makeMove

dance :: Int -> [DanceMove] -> String
dance n moves = wholeDance n moves 1

wholeDance :: Int -> [DanceMove] -> Int -> String
wholeDance n moves steps = foldl op initial [1..d]
  where
    op s _ = makeMoves s moves
    (ps, nps) = decompose moves
    initial = take n ['a'..]
    a = findCycle initial ps
    b = findCycle initial nps
    c = lcm a b
    d = steps `mod` c

decompose :: [DanceMove] -> ([DanceMove], [DanceMove])
decompose = partition isPartnerMove
  where
    isPartnerMove (Partner _ _) = True
    isPartnerMove _             = False

findCycle :: String -> [DanceMove] -> Int
findCycle s moves = length ss'
  where
    ss' = takeWhile (/= s) ss
    ss = drop 1 $ iterate f s
    f s = makeMoves s moves
