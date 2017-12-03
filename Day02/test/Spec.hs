import Day02
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day02 tests" $ do
      it "1122" $ do
        1 `shouldBe` 1
