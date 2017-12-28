module Day23 where

import           Data.Char       (isAlpha)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)
import           Debug.Trace

data Value =
  Reg Char |
  Val Int
  deriving (Eq, Show)

data Instruction =
  Set Char Value |
  Sub Char Value |
  Mul Char Value |
  Jnz Value Value
  deriving (Eq, Show)

type Regs = Map Char Int
type State = (Regs, Int, Int)

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
    "set" -> parseRegAndValue Set bits
    "sub" -> parseRegAndValue Sub bits
    "mul" -> parseRegAndValue Mul bits
    "jnz" -> parseValueAndValue Jnz bits
  where
    bits = splitOn " " line

parseInput :: String -> [Instruction]
parseInput input = map parseLine $ filter (not . null) $ lines input

processInstruction :: State -> Instruction -> State
processInstruction state @ (regs, ip, numMuls) instruction =
  -- trace ("state: " ++ show state) $
  case instruction of

    Set r v -> fn2 r v (flip const) 0
    Sub r v -> fn2 r v (-) 0
    Mul r v -> fn2 r v (*) 1

    Jnz val1 val2 ->
      if v1 /= 0
        then (regs, ip + v2, numMuls)
        else (regs, succ ip, numMuls)
      where
        v1 = regOrVal val1
        v2 = regOrVal val2

    where
      regOrVal val = case val of
        Reg r -> Map.findWithDefault 0 r regs
        Val v -> v
      fn2 r v op inc = (regs', succ ip, numMuls + inc)
        where
          regs' = Map.insert r (v1 `op` v2) regs
          v1 = Map.findWithDefault 0 r regs
          v2 = regOrVal v

runCommon :: [Instruction] -> State -> State
runCommon instructions = loop
  where
    loop state @ (_, ip, _) =
      case maybeState' of
        Just state' -> loop state'
        Nothing     -> state
      where
        maybeState' = fmap (processInstruction state) maybeInstruction
        maybeInstruction = Map.lookup ip program
        program = Map.fromList $ zip [0..] instructions

runProgramWithDebugOn :: [Instruction] -> Int
runProgramWithDebugOn instructions = numMuls
  where
    initialState = (Map.empty, 0, 0)
    (_, _, numMuls) = runCommon instructions initialState

runProgramWithDebugOff :: [Instruction] -> Maybe Int
runProgramWithDebugOff instructions = Map.lookup 'h' regs
  where
    initialState = (Map.singleton 'a' 1, 0, 0)
    (regs, _, _) = runCommon instructions initialState
