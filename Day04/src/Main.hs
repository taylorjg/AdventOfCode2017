import           Day04 (validatePassPhrase1, validatePassPhrase2)

main :: IO ()
main = do
    input <- readFile "Day04/src/input.txt"
    let passPhrases = lines input

    let results1 = map validatePassPhrase1 passPhrases
    let answer1 = length $ filter (== True) results1
    putStrLn $ "answer1: " ++ show answer1

    let results2 = map validatePassPhrase2 passPhrases
    let answer2 = length $ filter (== True) results2
    putStrLn $ "answer2: " ++ show answer2
