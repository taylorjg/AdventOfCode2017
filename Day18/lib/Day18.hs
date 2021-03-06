module Day18 where

import           Data.Char       (isAlpha)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)

data Value =
  Reg Char |
  Val Int
  deriving (Eq, Show)

data Instruction =
  Add Char Value |
  Set Char Value |
  Mod Char Value |
  Mul Char Value |
  Snd Value |
  Rcv Char |
  Jgz Value Value
  deriving (Eq, Show)

parseReg :: (Char -> Instruction) -> [String] -> Instruction
parseReg mkInstruction bits = mkInstruction reg
  where
    reg = head $ bits !! 1

parseVal :: (Value -> Instruction) -> [String] -> Instruction
parseVal mkInstruction bits = mkInstruction val
  where
    val = parseValue $ bits !! 1

parseRegAndValue :: (Char -> Value -> Instruction) -> [String] -> Instruction
parseRegAndValue mkInstruction bits = mkInstruction reg val
  where
    reg = head $ bits !! 1
    val = parseValue $ bits !! 2

parseValueAndValue :: (Value -> Value -> Instruction) -> [String] -> Instruction
parseValueAndValue mkInstruction bits = mkInstruction val1 val2
  where
    val1 = parseValue $ bits !! 1
    val2 = parseValue $ bits !! 2

parseValue :: String -> Value
parseValue s = case head s of
  ch | isAlpha ch -> Reg ch
  _  -> Val $ read s

parseLine :: String -> Instruction
parseLine line = case head bits of
    "add" -> parseRegAndValue Add bits
    "set" -> parseRegAndValue Set bits
    "mod" -> parseRegAndValue Mod bits
    "mul" -> parseRegAndValue Mul bits
    "snd" -> parseVal Snd bits
    "rcv" -> parseReg Rcv bits
    "jgz" -> parseValueAndValue Jgz bits
  where
    bits = splitOn " " line

parseInput :: String -> [Instruction]
parseInput input = map parseLine $ filter (not . null) $ lines input

type Regs = Map Char Int
type State1 = (Regs, Int, Int, Maybe Int)

processInstruction1 :: State1 -> Instruction -> State1
processInstruction1 (regs, ip, lastFreq, rcvFreq) instruction =
  case instruction of

    Add r v -> fn2 r v (+)
    Set r v -> fn2 r v (flip const)
    Mul r v -> fn2 r v (*)
    Mod r v -> fn2 r v mod

    Snd val          -> (regs, succ ip, freq, rcvFreq)
      where
        freq = regOrVal val

    Rcv r          ->
      if rv > 0
        then (regs, succ ip, lastFreq, Just lastFreq)
        else (regs, succ ip, lastFreq, rcvFreq)
      where
        rv = Map.findWithDefault 0 r regs

    Jgz val1 val2 ->
      if v1 > 0
        then (regs, ip + v2, lastFreq, rcvFreq)
        else (regs, succ ip, lastFreq, rcvFreq)
      where
        v1 = regOrVal val1
        v2 = regOrVal val2

    where
      regOrVal val = case val of
        Reg r -> Map.findWithDefault 0 r regs
        Val v -> v
      fn2 r v op = (regs', succ ip, lastFreq, rcvFreq)
        where
          regs' = Map.insert r (v1 `op` v2) regs
          v1 = Map.findWithDefault 0 r regs
          v2 = regOrVal v

runProgram :: [Instruction] -> Maybe Int
runProgram instructions = loop initialState
  where
    initialState = (Map.empty, 0, 0, Nothing)
    loop state @ (regs, ip, freq, maybeLastFreq) =
      case maybeState' of
        Just (_, _, _, Just rcvFreq) -> Just rcvFreq
        Just state'                  -> loop state'
        Nothing                      -> Nothing
      where
        maybeState' = fmap (processInstruction1 state) maybeInstruction
        maybeInstruction = Map.lookup ip program
        program = Map.fromList $ zip [0..] instructions

data State2 = State2 {
  regs    :: Regs,
  ip      :: Int,
  numSnds :: Int,
  pid     :: Int,
  blocked :: Bool
  } deriving (Eq, Ord, Show)

mkState :: Int -> State2
mkState pid = State2 {
  regs = Map.singleton 'p' pid,
  ip = 0,
  numSnds = 0,
  pid = pid,
  blocked = False
}

type RvcQueues = Map Int [Int]

processInstruction2 :: State2 -> RvcQueues -> Instruction -> (State2, RvcQueues)
processInstruction2 state rcvQueues instruction =
  case instruction of

    Add r v -> fn2 r v (+)
    Set r v -> fn2 r v (flip const)
    Mul r v -> fn2 r v (*)
    Mod r v -> fn2 r v rem

    Snd val          -> (state { ip = succ (ip state), numSnds = succ (numSnds state) }, rcvQueues')
      where
        v = regOrVal val
        rcvQueues' = Map.adjust (++ [v]) otherPid rcvQueues

    Rcv r          ->
      case q of
        v:vs -> (state { regs = regs', ip = succ (ip state) }, rcvQueues')
          where
            regs' = Map.insert r v (regs state)
            rcvQueues' = Map.insert thisPid vs rcvQueues
        []   -> (state { blocked = True }, rcvQueues)
      where
        q = Map.findWithDefault [] thisPid rcvQueues

    Jgz val1 val2 ->
      if v1 > 0
        then (state { ip = ip state + v2 }, rcvQueues)
        else (state { ip = succ (ip state) }, rcvQueues)
      where
        v1 = regOrVal val1
        v2 = regOrVal val2

    where
      thisPid = pid state
      otherPid = if thisPid == 0 then 1 else 0
      regOrVal val = case val of
        Reg r -> Map.findWithDefault 0 r (regs state)
        Val v -> v
      fn2 r v op = (state { regs = regs', ip = succ (ip state) }, rcvQueues)
        where
          regs' = Map.insert r (v1 `op` v2) (regs state)
          v1 = Map.findWithDefault 0 r (regs state)
          v2 = regOrVal v

runPrograms :: [Instruction] -> (State2, State2)
runPrograms instructions =
  loop s0 s1 rvcQueues
  where
    s0 = mkState 0
    s1 = mkState 1
    rvcQueues = Map.fromList [(0, []), (1, [])]
    loop s0 s1 rcvq0 =
      case (ms0, ms1) of
        (Just s0', Just s1') | blocked s0' && blocked s1' -> (s0', s1')
        (Nothing, Nothing)   -> (s0, s1)
        (Just s0', Just s1') -> loop s0' s1' rcvq2
        (Nothing, Just s1')  -> loop s0 s1' rcvq2
        (Just s0', Nothing)  -> loop s0' s1 rcvq2
      where
        m0 = advance s0 rcvq0
        rcvq1 = maybe rcvq0 snd m0
        m1 = advance s1 rcvq1
        rcvq2 = maybe rcvq1 snd m1
        ms0 = fmap fst m0
        ms1 = fmap fst m1
    advance state rcvq =
      fmap (processInstruction2 unblockedState rcvq) maybeInstruction
      where
        unblockedState = state { blocked = False }
        maybeInstruction = Map.lookup (ip state) program
    program = Map.fromList $ zip [0..] instructions

runTwoPrograms :: [Instruction] -> Int
runTwoPrograms instructions = numSnds s1
  where
    (s0, s1) = runPrograms instructions
