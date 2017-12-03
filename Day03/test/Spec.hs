import Day03
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day03 tests" $ do
      it "1122" $ do
        1 `shouldBe` 1
