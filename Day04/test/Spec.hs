import           Day04      (validatePassPhrase)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day04 tests" $

    describe "Part 1" $ do

      it "aa bb cc dd ee is valid" $
         validatePassPhrase "aa bb cc dd ee" `shouldBe` True

      it "aa bb cc dd aa is not valid" $
        validatePassPhrase "aa bb cc dd aa" `shouldBe` False

      it "aa bb cc dd aaa is valid" $
        validatePassPhrase "aa bb cc dd aaa" `shouldBe` True
