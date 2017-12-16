import           Data.Char (isSpace)
import           Day16

main :: IO ()
main = do
  input <- readFile "Day16/src/input.txt"
  let cleanedInput = filter (not . isSpace) input
  let danceMoves = parseDanceMoves cleanedInput

  let answer1 = dance 16 danceMoves
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = wholeDance 16 danceMoves 1000000 -- 1000000000
  putStrLn $ "answer2: " ++ show answer2
