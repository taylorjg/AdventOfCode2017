import           Day24

main :: IO ()
main = do
  input <- readFile "Day24/src/input.txt"
  let components = parseInput input

  let answer1 = computePart1 components
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = computePart2 components
  putStrLn $ "answer2: " ++ show answer2
