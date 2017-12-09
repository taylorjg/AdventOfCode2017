import           Day08

main :: IO ()
main = do
  input <- readFile "Day08/src/input.txt"
  let inputLines = lines input
  let instructions = parseInputLines inputLines
  let answer = processInstructions instructions
  putStrLn $ "answer: " ++ show answer
