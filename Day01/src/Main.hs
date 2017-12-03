import Day01 (captcha)

import Data.Char (isDigit)

main :: IO ()
main = do
    input <- readFile "Day01/src/input.txt"
    let cleanedInput = filter isDigit input
    let answer = captcha cleanedInput
    putStrLn $ "answer: " ++ show answer
