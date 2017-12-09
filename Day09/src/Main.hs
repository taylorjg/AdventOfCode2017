import           Day09

main :: IO ()
main = do
  input <- readFile "Day09/src/input.txt"
  let answer1 = scoreGroups input
  putStrLn $ "answer1: " ++ show answer1
