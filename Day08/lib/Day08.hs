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

processInstructions :: [Instruction] -> (String, Int, Maybe Int)
processInstructions instructions =
  (maxRegName, maxRegValue, maybeBiggestEver)
  where
    (m, maybeBiggestEver) = foldl processInstruction (Map.empty, Nothing) instructions
    (maxRegName, maxRegValue) = maxRegister m

processInstruction :: (Map String Int, Maybe Int) -> Instruction -> (Map String Int, Maybe Int)
processInstruction (m, maybeBiggestEver) instruction =
  (m', maybeBiggestEver')
    where
      rn1 = comparisonReg instruction
      rn2 = reg instruction
      rv1 = Map.findWithDefault 0 rn1 m
      rv2 = Map.findWithDefault 0 rn2 m
      conditonMet = evaluateCondition rv1 (comparisonOp instruction) (comparisonValue instruction)
      m' = if conditonMet
        then Map.insert rn2 rv2' m
        else Map.insert rn2 rv2 m
      rv2' = case op instruction of
        Inc -> rv2 + value instruction
        Dec -> rv2 - value instruction
      (_, maxRegValue) = maxRegister m'
      maybeBiggestEver' = case maybeBiggestEver of
        Nothing      -> Just maxRegValue
        Just biggest | maxRegValue > biggest -> Just maxRegValue
        current      -> current

evaluateCondition :: Int -> Comparison -> Int ->  Bool
evaluateCondition r op val =
  case op of
    GreaterThan      -> r > val
    GreaterThanEqual -> r >= val
    LessThan         -> r < val
    LessThanEqual    -> r <= val
    Equal            -> r == val
    NotEqual         -> r /= val

maxRegister :: Map String Int -> (String, Int)
maxRegister = maximumBy (\a b -> snd a `compare` snd b) . Map.assocs
