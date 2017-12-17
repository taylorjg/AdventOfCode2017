import           Day17
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day17 tests" $ do

    describe "Part 1" $

      it "3 steps" $
        spinlock1 3 `shouldBe` 638

    describe "Part 2" $

      it "3 steps" $
        spinlock2 3 `shouldBe` 1222153
