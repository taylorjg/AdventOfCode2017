import           Day02 (checksum)

main :: IO ()
main = do
  input <- readFile "Day02/src/input.txt"
  let ls = lines input
  let wss = map words ls
  let xss = map wordsToInts wss
  let answer = checksum xss
  putStrLn $ "answer: " ++ show answer
  where
    wordsToInts = map wordToInt
    wordToInt w = read w :: Int
