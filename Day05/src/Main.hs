import           Day05 (escapeMaze1, escapeMaze2)

main :: IO ()
main = do
  input <- readFile "Day05/src/input.txt"
  let jumps = map read . lines $ input :: [Int]

  let answer1 = escapeMaze1 jumps
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = escapeMaze2 jumps
  putStrLn $ "answer2: " ++ show answer2
