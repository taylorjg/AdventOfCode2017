import           Day23

main :: IO ()
main = do
  input <- readFile "Day23/src/input.txt"
  let instructions = parseInput input

  let answer1 = runProgramWithDebugOn instructions
  putStrLn $ "answer1: " ++ show answer1

  -- let answer2 = runProgramWithDebugOff instructions
  -- putStrLn $ "answer2: " ++ show answer2
