import           Day07 (bottomProgramName, parseInputLines, rebalancedWeight)

main :: IO ()
main = do
  input <- readFile "Day07/src/input.txt"
  let inputLines = lines input
  let programs = parseInputLines inputLines

  let answer1 = bottomProgramName programs
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = rebalancedWeight programs
  putStrLn $ "answer2: " ++ show answer2
