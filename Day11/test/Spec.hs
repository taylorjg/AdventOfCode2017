import           Day11
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day11 tests" $

    describe "Part 1" $ do

      it "ne,ne,ne" $
        3 `shouldBe` 3

      it "ne,ne,sw,sw" $
        0 `shouldBe` 0

      it "ne,ne,s,s" $
        2 `shouldBe` 2

      it "se,sw,se,sw,sw" $
       3 `shouldBe` 3
