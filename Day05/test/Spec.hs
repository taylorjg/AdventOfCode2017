import           Day05      (escapeMaze)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day05 tests" $

    describe "Part 1" $

      it "given example" $
        escapeMaze [0, 3, 0, 1, -3] `shouldBe` 5
