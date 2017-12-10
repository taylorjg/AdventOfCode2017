import           Day10
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day10 tests" $ do

    describe "Part 1" $

      it "given example" $
        processList 5 [3, 4, 1, 5] `shouldBe` 12

    describe "Part 2" $ do

      it "makeDenseHash" $
        makeDenseHash [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22] `shouldBe` [64]

      it "intsToHexString" $
        intsToHexString [32, 12] `shouldBe` "200c"

      it "empty string" $
        knotHash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"

      it "AoC 2017" $
        knotHash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"

      it "1,2,3" $
        knotHash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"

      it "1,2,4" $
        knotHash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
