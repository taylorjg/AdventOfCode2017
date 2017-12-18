import           Day18

main :: IO ()
main = do
  input <- readFile "Day18/src/input.txt"
  let instructions = parseInput input

  let answer1 = runProgram instructions
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = runTwoPrograms instructions
  putStrLn $ "answer2: " ++ show answer2
