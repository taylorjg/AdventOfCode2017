import           Data.List.Split (splitOn)
import           Day10

main :: IO ()
main = do
  input <- readFile "Day10/src/input.txt"
  let xs = map read $ splitOn "," input :: [Int]
  let answer1 = processList 256 xs
  putStrLn $ "answer1: " ++ show answer1
