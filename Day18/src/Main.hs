import           Day18

main :: IO ()
main = do
  input <- readFile "Day18/src/input.txt"
  let instructions = parseInput input
  print instructions
