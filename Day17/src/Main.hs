import           Day17

main :: IO ()
main = do
  let answer1 = spinlock1 301
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = spinlock2 301
  putStrLn $ "answer2: " ++ show answer2
