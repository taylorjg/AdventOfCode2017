import           Day02 (checksum1, checksum2)

readInputFile :: String -> IO [[Int]]
readInputFile fn = do
  input <- readFile fn
  let ls = lines input
  let wss = map words ls
  let xss = map wordsToInts wss
  return xss
  where
    wordsToInts = map wordToInt
    wordToInt w = read w :: Int


main :: IO ()
main = do
  xss <- readInputFile "Day02/src/input.txt"

  let answer1 = checksum1 xss
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = checksum2 xss
  putStrLn $ "answer2: " ++ show answer2
