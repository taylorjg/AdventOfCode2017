import           Day08

main :: IO ()
main = do
  input <- readFile "Day08/src/input.txt"
  let inputLines = lines input
  let instructions = parseInputLines inputLines

  let answer1 = processInstructions instructions
  putStrLn $ "answer1: " ++ show answer1
