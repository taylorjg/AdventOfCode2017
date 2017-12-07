import           Day07 (bottomProgramName, parseInputLines)

main :: IO ()
main = do
  input <- readFile "Day07/src/input.txt"
  let inputLines = lines input
  let programs = parseInputLines inputLines
  let answer1 = bottomProgramName programs
  putStrLn $ "answer1: " ++ show answer1
