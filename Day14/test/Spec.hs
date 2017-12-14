import           Day14
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day14 tests" $ do

    describe "Part 1" $

      it "count of squares used" $
        squaresUsed "flqrgnkx" `shouldBe` 8108

    describe "Part 2" $

      it "count of regions" $
        countRegions "flqrgnkx" `shouldBe` 1242
