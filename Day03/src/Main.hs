import           Day03 (spiralDistance)

main :: IO ()
main = do
    let answer1 = spiralDistance 289326
    putStrLn $ "answer1: " ++ show answer1
