import Day01 (captcha)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day01 tests" $ do

      it "1122" $ do
        captcha "1122" `shouldBe` 3
    
      it "1111" $ do
        captcha "1111" `shouldBe` 4
    
      it "1234" $ do
        captcha "1234" `shouldBe` 0
    
      it "91212129" $ do
        captcha "91212129" `shouldBe` 9
