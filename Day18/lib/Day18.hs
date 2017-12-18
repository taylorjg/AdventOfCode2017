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
  Snd Char |
  Rcv Char |
  Jgz Char Value
  deriving (Eq, Show)

parseOneOperand :: (Char -> Instruction) -> [String] -> Instruction
parseOneOperand mkInstruction bits = mkInstruction reg
  where
    reg = head $ bits !! 1

parseTwoOperands :: (Char -> Value -> Instruction) -> [String] -> Instruction
parseTwoOperands mkInstruction bits = mkInstruction reg val
  where
    reg = head $ bits !! 1
    val = parseValue $ bits !! 2

parseValue :: String -> Value
parseValue s = case head s of
  ch | isAlpha ch -> Reg ch
  _  -> Val $ read s

parseLine :: String -> Instruction
parseLine line = case head bits of
    "add" -> parseTwoOperands Add bits
    "set" -> parseTwoOperands Set bits
    "mod" -> parseTwoOperands Mod bits
    "mul" -> parseTwoOperands Mul bits
    "snd" -> parseOneOperand Snd bits
    "rcv" -> parseOneOperand Rcv bits
    "jgz" -> parseTwoOperands Jgz bits
  where
    bits = splitOn " " line

parseInput :: String -> [Instruction]
parseInput input = map parseLine $ filter (not . null) $ lines input

type Regs = Map Char Int
type State = (Regs, Int, Int, Maybe Int)

processInstruction :: State -> Instruction -> State
processInstruction (regs, ip, lastFreq, rcvFreq) instruction =
  case instruction of

    Add r v -> fn2 r v (+)
    Set r v  -> fn2 r v (flip const)
    Mul r v -> fn2 r v (*)
    Mod r v  -> fn2 r v mod

    Snd r          -> (regs, succ ip, freq, rcvFreq)
      where
        freq = Map.findWithDefault 0 r regs

    Rcv r          ->
      if rv > 0
        then (regs, succ ip, lastFreq, Just lastFreq)
        else (regs, succ ip, lastFreq, rcvFreq)
      where
        rv = Map.findWithDefault 0 r regs

    Jgz r (Reg r2) ->
      if rv > 0
        then (regs, ip + rv2, lastFreq, rcvFreq)
        else (regs, succ ip, lastFreq, rcvFreq)
       where
        rv = Map.findWithDefault 0 r regs
        rv2 = Map.findWithDefault 0 r2 regs

    Jgz r (Val v)  ->
      if rv > 0
        then (regs, ip + v, lastFreq, rcvFreq)
        else (regs, succ ip, lastFreq, rcvFreq)
      where
        rv = Map.findWithDefault 0 r regs

    where
      fn2 r v op = (regs', succ ip, lastFreq, rcvFreq)
        where
          regs' = Map.insert r (v1 `op` v2) regs
          v1 = Map.findWithDefault 0 r regs
          v2 = case v of
            Reg r -> Map.findWithDefault 0 r regs
            Val v -> v

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
        maybeState' = fmap (processInstruction state) maybeInstruction
        maybeInstruction = Map.lookup ip program
        program = Map.fromList $ zip [0..] instructions

type RvcQueues = Map Int [Int]

data State3 = State3 {
  regs    :: Regs,
  ip      :: Int,
  numSnds :: Int,
  pid     :: Int,
  blocked :: Bool
  } deriving (Eq, Ord)

processInstruction2 :: State3 -> RvcQueues -> Instruction -> (State3, RvcQueues)
processInstruction2 state rcvQueues instruction =
  case instruction of

    Add r v -> fn2 r v (+)
    Set r v  -> fn2 r v (flip const)
    Mul r v -> fn2 r v (*)
    Mod r v  -> fn2 r v mod

    Snd r          -> (state { ip = succ (ip state), numSnds = succ (numSnds state) }, rcvQueues')
      where
        v = Map.findWithDefault 0 r (regs state)
        -- Map.update ?
        rcvQueues' = Map.insert otherPid q' rcvQueues
        q = Map.findWithDefault [] otherPid rcvQueues
        q' = q ++ [v]

    Rcv r          ->
      case q of
        v:vs -> (state { regs = regs', ip = succ (ip state), blocked = False }, rcvQueues')
          where
            regs' = Map.insert r v (regs state)
            rcvQueues' = Map.insert thisPid vs rcvQueues
        []   -> (state { blocked = True }, rcvQueues)
      where
        q = Map.findWithDefault [] thisPid rcvQueues

    Jgz r (Reg r2) ->
      if rv > 0
        then (state { ip = ip state + rv2 }, rcvQueues)
        else (state { ip = succ (ip state) }, rcvQueues)
       where
        rv = Map.findWithDefault 0 r (regs state)
        rv2 = Map.findWithDefault 0 r2 (regs state)

    Jgz r (Val v)  ->
      if rv > 0
        then (state { ip = ip state + v }, rcvQueues)
        else (state { ip = succ (ip state) }, rcvQueues)
      where
        rv = Map.findWithDefault 0 r (regs state)

    where
      thisPid = pid state
      otherPid = if thisPid == 0 then 1 else 0
      fn2 r v op = (state { regs = regs', ip = succ (ip state) }, rcvQueues)
        where
          regs' = Map.insert r (v1 `op` v2) (regs state)
          v1 = Map.findWithDefault 0 r (regs state)
          v2 = case v of
            Reg r -> Map.findWithDefault 0 r (regs state)
            Val v -> v

mkState :: Int -> State3
mkState pid = State3 {
  regs = Map.singleton 'p' pid,
  ip = 0,
  numSnds = 0,
  pid = pid,
  blocked = False
}

runPrograms :: [Instruction] -> (State3, State3)
runPrograms instructions =
  loop s0 s1 rvcQueues
  where
    s0 = mkState 0
    s1 = mkState 1
    rvcQueues = Map.fromList [(0, []), (1, [])]
    loop s0 s1 rcvq =
      case (ms0, ms1) of
        (Just s0', Just s1') | blocked s0' && blocked s1' -> (s0', s1') -- deadlock
        (Nothing, Nothing)   -> (s0, s1)
        (Just s0', Just s1') -> loop s0' s1' rcvq2
        (Nothing, Just s1')  -> loop s0 s1' rcvq2
        (Just s0', Nothing)  -> loop s0' s1 rcvq2
      where
        m0 = advance s0 rcvq
        rcvq1 = maybe rcvq snd m0
        m1 = advance s1 rcvq1
        rcvq2 = maybe rcvq1 snd m1
        ms0 = fmap fst m0
        ms1 = fmap fst m1
    advance state rcvQueues =
      fmap (processInstruction2 unblockedState rcvQueues) maybeInstruction
      where
        unblockedState = state { blocked = False }
        maybeInstruction = Map.lookup (ip state) program
    program = Map.fromList $ zip [0..] instructions

runTwoPrograms :: [Instruction] -> Int
runTwoPrograms instructions = numSnds s1
  where
    (s0, s1) = runPrograms instructions
