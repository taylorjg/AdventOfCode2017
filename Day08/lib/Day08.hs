module Day08 where

import           Data.List       (maximumBy)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Text.Regex.Base
import           Text.Regex.PCRE

data Operation =
  Inc |
  Dec
  deriving (Eq, Show)

data Comparison =
  GreaterThan |
  GreaterThanEqual |
  LessThan |
  LessThanEqual |
  Equal |
  NotEqual
  deriving (Eq, Show)

data Instruction = Instruction {
  reg             :: String,
  op              :: Operation,
  value           :: Int,
  comparisonReg   :: String,
  comparisonOp    :: Comparison,
  comparisonValue :: Int
  } deriving (Eq, Show)

parseInputLines :: [String] -> [Instruction]
parseInputLines = map parseInputLine

parseInputLine :: String -> Instruction
parseInputLine s =
  Instruction reg op value comparisonReg comparisonOp comparisonValue
  where
    [matches] = s =~ pat :: [[String]]
    reg = matches !! 1
    op = strToOperation $ matches !! 2
    value = strToValue $ matches !! 3
    comparisonReg = matches !! 4
    comparisonOp = strToComparison $ matches !! 5
    comparisonValue = strToValue $ matches !! 6
    pat = "(\\w+)\\s(dec|inc)\\s(-?\\d+)\\sif\\s(\\w+)\\s(>|>=|<|<=|==|!=)\\s(-?\\d+)"

strToValue :: String -> Int
strToValue = read

strToOperation :: String -> Operation
strToOperation s =
  case s of
    "inc" -> Inc
    "dec" -> Dec

strToComparison :: String -> Comparison
strToComparison s =
  case s of
    ">"  -> GreaterThan
    ">=" -> GreaterThanEqual
    "<"  -> LessThan
    "<=" -> LessThanEqual
    "==" -> Equal
    "!=" -> NotEqual

processInstructions :: [Instruction] -> (String, Int)
processInstructions instructions =
  maxReg
  where
    m = foldl processInstruction Map.empty instructions
    kvps = Map.assocs m
    maxReg = maximumBy (\a b -> snd a `compare` snd b) kvps

processInstruction :: Map String Int -> Instruction -> Map String Int
processInstruction m instruction =
  result
    where
      r1 = Map.findWithDefault 0 (comparisonReg instruction) m
      r2 = Map.findWithDefault 0 (reg instruction) m
      conditonMet = evaluateCondition r1 (comparisonOp instruction) (comparisonValue instruction)
      result = if conditonMet then Map.insert (reg instruction) r2' m else m
      r2' = case op instruction of
        Inc -> r2 + value instruction
        Dec -> r2 - value instruction

evaluateCondition :: Int -> Comparison -> Int ->  Bool
evaluateCondition r op val =
  case op of
    GreaterThan      -> r > val
    GreaterThanEqual -> r >= val
    LessThan         -> r < val
    LessThanEqual    -> r <= val
    Equal            -> r == val
    NotEqual         -> r /= val
