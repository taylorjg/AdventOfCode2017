module Day25 where

import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.List.Split (chunksOf, splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Text.Regex.Base
import           Text.Regex.PCRE

data Direction = L | R deriving (Show, Read, Eq, Ord)

data Rule = Rule {
  value         :: Int,
  direction     :: Direction,
  nextStateName :: String
  } deriving (Show, Eq, Ord)

data State = State {
  name  :: String,
  rule0 :: Rule,
  rule1 :: Rule
  } deriving (Show, Eq, Ord)

data Blueprint = Blueprint {
  numSteps :: Int,
  states   :: [State]
  } deriving (Show, Eq, Ord)

getDirection :: String -> Direction
getDirection s = case s of
  "left"  -> L
  "right" -> R
  _       -> error "Unknown direction"

parseRule :: [String] -> Rule
parseRule lines = Rule value direction nextStateName
  where
    [matches1] = head lines =~ pat1 :: [[String]]
    [matches2] = (lines !! 1) =~ pat2 :: [[String]]
    [matches3] = (lines !! 2) =~ pat3 :: [[String]]
    value = read $ matches1 !! 1
    direction = getDirection $ matches2 !! 1
    nextStateName = matches3 !! 1
    pat1 = "- Write the value (\\d+)."
    pat2 = "- Move one slot to the (\\w+)."
    pat3 = "- Continue with state (\\w+)."

parseState :: [String] -> State
parseState lines = State name rule0 rule1
  where
    [matches] = (lines !! 1) =~ pat :: [[String]]
    name = matches !! 1
    pat = "In state (\\w+):"
    rule0 = parseRule (take 3 $ drop 3 lines)
    rule1 = parseRule (take 3 $ drop 7 lines)

parseBlueprint :: String -> Blueprint
parseBlueprint input = Blueprint numSteps states
  where
    lines = splitOn "\n" input
    [matches] = (lines !! 1) =~ pat :: [[String]]
    numSteps = read $ matches !! 1
    pat = "Perform a diagnostic checksum after (\\d+) steps."
    states = parseState `fmap` chunks
    chunks = filter ((== 10) . length) $ chunksOf 10 $ drop 2 lines

runBlueprint :: Blueprint -> Int
runBlueprint blueprint = numOnes
  where
    pairs = (\state -> (name state, state)) `fmap` states blueprint
    statesMap = Map.fromList pairs
    initialState = (IntMap.empty, 0, "A")
    finalState = last $ take (numSteps blueprint) (iterate f initialState)
    (finalTape, _, _) =  finalState
    numOnes = IntMap.size $ IntMap.filter (== 1) finalTape
    f (tape, cursor, currState) = (tape', cursor', currState')
      where
        state = statesMap Map.! currState
        currValue = IntMap.findWithDefault 0 cursor tape
        rule = case currValue of
          0 -> rule0 state
          1 -> rule1 state
        tape' = IntMap.insert cursor (value rule) tape
        cursor' = case direction rule of
          L -> cursor - 1
          R -> cursor + 1
        currState' = nextStateName rule
