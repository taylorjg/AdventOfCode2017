import           Day05      (escapeMaze1, escapeMaze2)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day05 tests" $ do

    describe "Part 1" $

      it "given example" $
        escapeMaze1 [0, 3, 0, 1, -3] `shouldBe` 5

    describe "Part 2" $

      it "given example" $
        escapeMaze2 [0, 3, 0, 1, -3] `shouldBe` 10
