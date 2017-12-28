import           Day24
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day24 tests" $ do

    let components = [
            Component 0 0,
            Component 1 1
          ]

    describe "Part 1" $ do

      it "can parse input" $ do
        input <- readFile "Day24/test/input.txt"
        parseInput input `shouldBe` components

      it "can compute part 1" $
        computePart1 components `shouldBe` 0

    describe "Part 2" $

      it "can compute part 2" $
        computePart2 components `shouldBe` 0
