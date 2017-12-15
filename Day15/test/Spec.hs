import           Day15
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day15 tests" $

    describe "Part 1" $

      it "given example" $
        judge 65 8921 `shouldBe` 588
