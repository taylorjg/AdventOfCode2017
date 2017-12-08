import           Data.Map   (empty, fromList, singleton)
import           Day08
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day08 tests" $ do

    let instructions = [
            Instruction "b" Inc 5 "a" GreaterThan 1,
            Instruction "a" Inc 1 "b" LessThan 5,
            Instruction "c" Dec (-10) "a" GreaterThanEqual 1,
            Instruction "c" Inc (-20) "c" Equal 10
          ]

    describe "Part 1" $ do

      it "can parse input file" $ do
        input <- readFile "Day08/test/input.txt"
        let inputLines = lines input
        parseInputLines inputLines `shouldBe` instructions

      it "given example" $
        processInstructions instructions `shouldBe` ("a", 1)
