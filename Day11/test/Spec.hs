import           Day11
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day11 tests" $

    describe "Part 1" $ do

      it "ne,ne,ne" $
        shortestRoute "ne,ne,ne" `shouldBe` 3

      it "ne,ne,sw,sw" $
        shortestRoute "ne,ne,sw,sw" `shouldBe` 0

      it "ne,ne,s,s" $
        shortestRoute "ne,ne,s,s" `shouldBe` 2

      it "se,sw,se,sw,sw" $
        shortestRoute "se,sw,se,sw,sw" `shouldBe` 3
