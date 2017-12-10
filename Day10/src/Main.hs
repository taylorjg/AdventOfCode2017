import           Data.Char       (isSpace)
import           Data.List.Split (splitOn)
import           Day10

main :: IO ()
main = do
  input <- readFile "Day10/src/input.txt"
  let cleanedInput = filter (not . isSpace) input

  let xs = map read $ splitOn "," cleanedInput
  let answer1 = processList 256 xs
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = knotHash cleanedInput
  putStrLn $ "answer2: " ++ show answer2
