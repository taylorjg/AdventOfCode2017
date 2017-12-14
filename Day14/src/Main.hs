import           Day14

main :: IO ()
main = do
  let answer1 = squaresUsed "hxtvlmkl"
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = countRegions "hxtvlmkl"
  putStrLn $ "answer2: " ++ show answer2
