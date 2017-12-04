import           Day04 (validatePassPhrase)

main :: IO ()
main = do
    input <- readFile "Day04/src/input.txt"
    let passPhrases = lines input
    let results = map validatePassPhrase passPhrases
    let answer1 = length $ filter (== True) results
    putStrLn $ "answer1: " ++ show answer1
