import           Day06      (numRedistributions)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day06 tests" $

    describe "Part 1" $

      it "given example" $
        numRedistributions [0, 2, 7, 0] `shouldBe` 5
