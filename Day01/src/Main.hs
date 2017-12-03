import           Data.Char (isDigit)
import           Day01     (captcha1, captcha2)

main :: IO ()
main = do
  input <- readFile "Day01/src/input.txt"
  let cleanedInput = filter isDigit input

  let answer1 = captcha1 cleanedInput
  putStrLn $ "answer1: " ++ show answer1

  let answer2 = captcha2 cleanedInput
  putStrLn $ "answer2: " ++ show answer2
