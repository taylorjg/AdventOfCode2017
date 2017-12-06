import           Day06      (numRedistributions1, numRedistributions2)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day06 tests" $ do

    describe "Part 1" $

      it "given example" $
        numRedistributions1 [0, 2, 7, 0] `shouldBe` 5

    describe "Part 2" $

      it "given example" $
        numRedistributions2 [0, 2, 7, 0] `shouldBe` (4, [2, 4, 1, 2])
