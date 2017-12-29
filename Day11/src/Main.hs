import           Day11

main :: IO ()
main = do
  input <- readFile "Day11/src/input.txt"
  let path = head $ lines input

  let answer1 = shortestRoute path
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = furthestDistance path
  putStrLn $ "answer2: " ++ show answer2
