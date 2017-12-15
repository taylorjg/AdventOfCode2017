import           Day15
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day15 tests" $ do

    describe "Part 1" $

      it "given example" $
        judge1 65 8921 `shouldBe` 588

    describe "Part 2" $

      it "given example" $
        judge2 65 8921 `shouldBe` 309
