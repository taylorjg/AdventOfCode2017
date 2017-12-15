import           Day15

main :: IO ()
main = do
  let answer1 = judge1 873 583
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = judge2 873 583
  putStrLn $ "answer2: " ++ show answer2
