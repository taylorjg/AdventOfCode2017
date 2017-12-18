module Day18 where

import           Data.Char       (isAlpha)
import           Data.List.Split (splitOn)

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
