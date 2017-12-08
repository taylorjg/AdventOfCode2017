module Day07 (
  parseInputLines,
  bottomProgramName,
  rebalancedWeight,
  mkProgram) where

import           Data.Char       (isSpace)
import           Data.List       (find, groupBy, sortOn)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import qualified Data.Maybe      as Maybe
import           Text.Regex.Base
import           Text.Regex.PCRE

data Program = Program {
  name            :: String,
  weight          :: Int,
  subProgramNames :: [String],
  parent          :: Maybe Program
  } deriving (Eq, Show)

mkProgram :: String -> Int -> [String] -> Program
mkProgram name weight subProgramNames = Program name weight subProgramNames Nothing

parseInputLines :: [String] -> [Program]
parseInputLines = map parseInputLine

parseInputLine :: String -> Program
parseInputLine s =
  mkProgram name weight subProgramNames
  where
    [matches] = s =~ pat :: [[String]]
    name = matches !! 1
    weight = read $ matches !! 2 :: Int
    subProgramNames = removeEmptyStrings $ splitOn "," $ removeSpaces (matches !! 3)
    removeSpaces = filter (not . isSpace)
    removeEmptyStrings = filter (/= "")
    pat = "(\\w+)\\s\\((\\d+)\\)(?:\\s->\\s([\\w,\\s]+))?"

bottomProgramName :: [Program] -> Maybe String
bottomProgramName programs = name `fmap` maybeRoot
  where
    m = buildMap programs
    maybeRoot = getRoot m

getRoot :: Map String Program -> Maybe Program
getRoot = Map.foldl op2 Nothing
  where
    op2 acc p = case (acc, parent p) of
      (Nothing, Nothing) -> Just p
      _                  -> acc

buildMap :: [Program] -> Map String Program
buildMap programs = m2
  where
    m1 = Map.fromList $ zip (map name programs) programs
    m2 = foldl op1 m1 programs
    op1 m p =
      case maybeParent of
        Nothing -> m
        Just parent -> Map.insert n p' m
          where
            p' = p { parent = Just parent }
      where
        n = name p
        maybeParent = findParent n
    findParent name = find predicate programs
      where
        predicate p = name `elem` subProgramNames p

data Rose a = Rose a [Rose a]

rebalancedWeight :: [Program] -> Maybe (String, Int)
rebalancedWeight programs =
  maybeRoot >>= (checkTree . buildTree m)
  where
    m = buildMap programs
    maybeRoot = getRoot m

buildTree :: Map String Program -> Program -> Rose Program
buildTree m root =
  Rose root roseChildren
  where
    children = findChildPrograms m root
    roseChildren = map (buildTree m) children

sumTree :: Rose Program -> Int
sumTree tree =
  case tree of
    Rose p [] -> weight p
    Rose p children -> weight p + childWeights
      where
        childWeights = sum $ map sumTree children

checkTree :: Rose Program -> Maybe (String, Int)
checkTree tree =
  case tree of
    Rose _ []       -> Nothing
    Rose _ children ->
      case (oddOne, correctOnes) of
        ([[(subTree @ (Rose p _), oddWeight)]], [(_, correctWeight):_]) ->
          case checkTree subTree of
            Nothing -> Just (name p, adjustedWeight)
            result  -> result
            where
              diff = correctWeight - oddWeight
              adjustedWeight = weight p + diff
        _                             -> Nothing
      where
        childWeights = map sumTree children
        zipped = zip children childWeights
        zippedSorted = sortOn snd zipped
        grouped = groupBy (\(_, w1) (_, w2) -> w1 == w2) zippedSorted
        oddOne = filter (\g -> length g == 1) grouped
        correctOnes = filter (\g -> length g > 1) grouped

findChildPrograms :: Map String Program -> Program -> [Program]
findChildPrograms m = Maybe.mapMaybe (`Map.lookup` m) . subProgramNames
