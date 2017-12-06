import           Day06 (numRedistributions1, numRedistributions2)

main :: IO ()
main = do
  input <- readFile "Day06/src/input.txt"
  let banks = map read . words $ input :: [Int]

  let answer1 = numRedistributions1 banks
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = numRedistributions2 banks
  putStrLn $ "answer2: " ++ show answer2
