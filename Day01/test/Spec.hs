import           Day01      (captcha1, captcha2)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day01 tests" $ do

    describe "Part 1" $ do

      it "1122" $
        captcha1 "1122" `shouldBe` 3

      it "1111" $
        captcha1 "1111" `shouldBe` 4

      it "1234" $
        captcha1 "1234" `shouldBe` 0

      it "91212129" $
        captcha1 "91212129" `shouldBe` 9

    describe "Part 2" $ do

      it "1212" $
        captcha2 "1212" `shouldBe` 6

      it "1221" $
        captcha2 "1221" `shouldBe` 0

      it "123425" $
        captcha2 "123425" `shouldBe` 4

      it "123123" $
        captcha2 "123123" `shouldBe` 12

      it "12131415" $
        captcha2 "12131415" `shouldBe` 4
