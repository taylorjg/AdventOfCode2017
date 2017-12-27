import           Day25

main :: IO ()
main = do
  input <- readFile "Day25/src/input.txt"
  let blueprint = parseBlueprint input
  let answer1 = runBlueprint blueprint
  putStrLn $ "answer1: " ++ show answer1
