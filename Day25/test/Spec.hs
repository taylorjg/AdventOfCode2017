import           Day25
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day25 tests" $

    describe "Part 1" $ do

      let blueprint = Blueprint 6 [
              State "A" (Rule 1 R "B") (Rule 0 L "B"),
              State "B" (Rule 1 L "A") (Rule 1 R "A")
            ]

      it "can parse input" $ do
        input <- readFile "Day25/test/input.txt"
        parseBlueprint input `shouldBe` blueprint

      it "can run blueprint" $
        runBlueprint blueprint `shouldBe` 3
