import           Day03      (firstValueWrittenBiggerThan, spiralDistance)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day03 tests" $ do

    describe "Part 1" $ do

      it "spiralDistance 1" $
        spiralDistance 1 `shouldBe` 0

      it "spiralDistance 12" $
        spiralDistance 12 `shouldBe` 3

      it "spiralDistance 23" $
        spiralDistance 23 `shouldBe` 2

      it "spiralDistance 1024" $
        spiralDistance 1024 `shouldBe` 31

    describe "Part 2" $ do

      it "firstValueWrittenBiggerThan 54" $
        firstValueWrittenBiggerThan 54 `shouldBe` 57

      it "firstValueWrittenBiggerThan 362" $
        firstValueWrittenBiggerThan 362 `shouldBe` 747
