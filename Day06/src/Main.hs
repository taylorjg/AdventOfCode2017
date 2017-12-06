import           Day06 (numRedistributions)

main :: IO ()
main = do
  input <- readFile "Day06/src/input.txt"
  let banks = map read . words $ input :: [Int]

  let answer1 = numRedistributions banks
  putStrLn $ "answer1: " ++ show answer1
