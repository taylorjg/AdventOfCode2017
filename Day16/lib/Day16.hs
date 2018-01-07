module Day16 (
  DanceMove(..),
  parseDanceMoves,
  dance,
  wholeDance) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List                   (partition)
import           Data.List.Split             (splitOn)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

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

makeSpinMoveST :: PrimMonad m => MV.MVector (PrimState m) Char -> Int -> m ()
makeSpinMoveST mv x = do
  MV.unsafeMove dst1 src1
  MV.unsafeMove dst2 src2
  where
    n = MV.length mv `div` 2
    idx = n - x
    src1 = MV.unsafeSlice 0 n mv
    dst1 = MV.unsafeSlice n n mv
    src2 = MV.unsafeSlice idx n mv
    dst2 = MV.unsafeSlice 0 n mv

makeExchangeMoveST :: PrimMonad m => MV.MVector (PrimState m) Char -> Int -> Int -> m ()
makeExchangeMoveST = MV.unsafeSwap

findCharsST :: PrimMonad m => MV.MVector (PrimState m) Char -> Char -> Char -> m (Int, Int)
findCharsST mv cha chb = go 0 Nothing Nothing
  where
    go idx midxa midxb = do
      ch <- MV.unsafeRead mv idx
      let idx' = succ idx
      case (midxa, midxb) of
        (_, Just idxb) | ch == cha -> return (idx, idxb)
        (_, Nothing)   | ch == cha -> go idx' (Just idx) Nothing
        (Just idxa, _) | ch == chb -> return (idxa, idx)
        (Nothing, _)   | ch == chb -> go idx' Nothing (Just idx)
        _              -> go idx' midxa midxb

makePartnerMoveST :: PrimMonad m => MV.MVector (PrimState m) Char -> Char -> Char -> m ()
makePartnerMoveST mv cha chb = do
  (idxa, idxb) <- findCharsST mv cha chb
  MV.unsafeSwap mv idxa idxb

makeMoveST :: PrimMonad m => MV.MVector (PrimState m) Char -> DanceMove -> m ()
makeMoveST mv move = case move of
  Spin x       -> makeSpinMoveST mv x
  Exchange a b -> makeExchangeMoveST mv a b
  Partner a b  -> makePartnerMoveST mv a b

makeMovesST :: PrimMonad m => MV.MVector (PrimState m) Char -> [DanceMove] -> m ()
makeMovesST mv = mapM_ (makeMoveST mv)

dance :: Int -> [DanceMove] -> String
dance n moves = wholeDance n moves 1

wholeDance :: Int -> [DanceMove] -> Int -> String
wholeDance n moves steps = take n $ V.toList $ runSteps v moves d
  where
    s = take n ['a'..]
    v = V.fromList $ s ++ s
    (ps, nps) = decompose moves
    a = findCycle s v ps
    b = findCycle s v nps
    c = lcm a b
    d = steps `mod` c

decompose :: [DanceMove] -> ([DanceMove], [DanceMove])
decompose = partition isPartnerMove
  where
    isPartnerMove (Partner _ _) = True
    isPartnerMove _             = False

runSteps :: V.Vector Char -> [DanceMove] -> Int -> V.Vector Char
runSteps v moves steps = runST $ do
  mv <- V.thaw v
  mapM_ (\_ -> makeMovesST mv moves) [1..steps]
  V.unsafeFreeze mv

findCycle :: String -> V.Vector Char -> [DanceMove] -> Int
findCycle s v moves = length ss'
  where
    n = length s
    ss' = takeWhile ((/= s) . take n . V.toList) vs
    vs = drop 1 $ iterate f v
    -- TODO: try to avoid need for repeated thaw/freeze
    f v' = runST $ do
      mv <- V.thaw v'
      makeMovesST mv moves
      V.unsafeFreeze mv
