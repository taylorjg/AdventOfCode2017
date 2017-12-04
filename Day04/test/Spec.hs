import           Day04      (validatePassPhrase1, validatePassPhrase2)
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day04 tests" $ do

    describe "Part 1" $ do

      it "aa bb cc dd ee is valid" $
        validatePassPhrase1 "aa bb cc dd ee" `shouldBe` True

      it "aa bb cc dd aa is not valid" $
        validatePassPhrase1 "aa bb cc dd aa" `shouldBe` False

      it "aa bb cc dd aaa is valid" $
        validatePassPhrase1 "aa bb cc dd aaa" `shouldBe` True

    describe "Part 2" $ do

      it "abcde fghij is a valid passphrase" $
        validatePassPhrase2 "abcde fghij" `shouldBe` True

      it "abcde xyz ecdab is not valid" $
        validatePassPhrase2 "abcde xyz ecdab" `shouldBe` False

      it "a ab abc abd abf abj is a valid passphrase" $
        validatePassPhrase2 "a ab abc abd abf abj" `shouldBe` True

      it "iiii oiii ooii oooi oooo is valid" $
        validatePassPhrase2 "iiii oiii ooii oooi oooo" `shouldBe` True

      it "oiii ioii iioi iiio is not valid" $
        validatePassPhrase2 "oiii ioii iioi iiio" `shouldBe` False
