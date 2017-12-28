import           Day24
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day24 tests" $ do

    let components = [
            Component 0 2,
            Component 2 2,
            Component 2 3,
            Component 3 4,
            Component 3 5,
            Component 0 1,
            Component 10 1,
            Component 9 10
          ]

    describe "Part 1" $ do

      it "can parse input" $ do
        input <- readFile "Day24/test/input.txt"
        parseInput input `shouldBe` components

      it "can compute part 1" $
        computePart1 components `shouldBe` 31

    describe "Part 2" $

      it "can compute part 2" $
        computePart2 components `shouldBe` 19
