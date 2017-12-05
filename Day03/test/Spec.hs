import           Day03      (spiralDistance)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day03 tests" $

    describe "Part 1" $ do

      it "spiralDistance 1" $
        spiralDistance 1 `shouldBe` 0

      it "spiralDistance 12" $
        spiralDistance 12 `shouldBe` 3

      it "spiralDistance 23" $
        spiralDistance 23 `shouldBe` 2

      it "spiralDistance 1024" $
        spiralDistance 1024 `shouldBe` 31
