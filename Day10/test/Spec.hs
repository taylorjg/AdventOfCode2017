import           Day10
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day10 tests" $

    describe "Part 1" $

      it "given example" $
        processList 5 [3, 4, 1, 5] `shouldBe` 12
