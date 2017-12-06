import           Day05

main :: IO ()
main = do
  input <- readFile "Day05/src/input.txt"
  let jumps = map read . lines $ input :: [Int]

  let answer1 = escapeMaze jumps
  putStrLn $ "answer1: " ++ show answer1
