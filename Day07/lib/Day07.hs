module Day07 (parseInputLines, bottomProgramName, mkProgram) where

import           Data.Char       (isSpace)
import           Data.List       (find)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as Map
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
bottomProgramName programs =
  name `fmap` root
  where
    root = Map.foldl op2 Nothing m2
    op2 acc p = case (acc, parent p) of
      (Nothing, Nothing) -> Just p
      _                  -> acc
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
