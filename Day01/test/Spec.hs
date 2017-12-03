import Day01
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day01 tests" $ do
      it "1122" $ do
        1 `shouldBe` 1
