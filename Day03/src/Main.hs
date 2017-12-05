import           Day03 (firstValueWrittenBiggerThan, spiralDistance)

main :: IO ()
main = do
    let answer1 = spiralDistance 289326
    putStrLn $ "answer1: " ++ show answer1

    let answer2 = firstValueWrittenBiggerThan 289326
    putStrLn $ "answer2: " ++ show answer2
