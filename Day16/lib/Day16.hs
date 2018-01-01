module Day16 (
  DanceMove(..),
  parseDanceMoves,
  dance,
  wholeDance) where

import           Data.List       (elemIndex, partition)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromMaybe)
import           Data.Semigroup  ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T

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

makeSpinMove :: Text -> Int -> Text
makeSpinMove s x = s'
  where
    len = T.length s
    v1 = T.drop (len - x) s
    v2 = T.take (len - x) s
    s' = v1 <> v2

makeExchangeMove :: Text -> Int -> Int -> Text
makeExchangeMove s idxa idxb = s'
  where
    cha = T.index s idxa
    chb = T.index s idxb
    s' = snd $ T.mapAccumL op 0 s
    op idx ch = (succ idx, ch')
      where
        ch'
          | idx == idxa = chb
          | idx == idxb = cha
          | otherwise = ch

makePartnerMove :: Text -> Char -> Char -> Text
makePartnerMove s a b = makeExchangeMove s idxa idxb
  where
    idxa = fromMaybe 0 $ T.findIndex (== a) s
    idxb = fromMaybe 0 $ T.findIndex (== b) s

makeMove :: Text -> DanceMove -> Text
makeMove s m = case m of
  Spin x       -> makeSpinMove s x
  Exchange a b -> makeExchangeMove s a b
  Partner a b  -> makePartnerMove s a b

makeMoves :: Text -> [DanceMove] -> Text
makeMoves = foldl makeMove

dance :: Int -> [DanceMove] -> String
dance n moves = wholeDance n moves 1

wholeDance :: Int -> [DanceMove] -> Int -> String
wholeDance n moves steps = T.unpack $ foldl op initial [1..d]
  where
    op s _ = makeMoves s moves
    (ps, nps) = decompose moves
    initial = T.pack $ take n ['a'..]
    a = findCycle initial ps
    b = findCycle initial nps
    c = lcm a b
    d = steps `mod` c

decompose :: [DanceMove] -> ([DanceMove], [DanceMove])
decompose = partition isPartnerMove
  where
    isPartnerMove (Partner _ _) = True
    isPartnerMove _             = False

findCycle :: Text -> [DanceMove] -> Int
findCycle s moves = length ss'
  where
    ss' = takeWhile (/= s) ss
    ss = drop 1 $ iterate f s
    f s = makeMoves s moves
