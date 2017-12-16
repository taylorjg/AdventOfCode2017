import           Data.Char  (isSpace)
import           Day16
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day16 tests" $ do

    let danceMoves = [
            Spin 1,
            Exchange 3 4,
            Partner 'e' 'b'
          ]

    describe "Part 1" $ do

      it "can parse dance moves" $ do
        input <- readFile "Day16/test/input.txt"
        let cleanedInput = filter (not . isSpace) input
        parseDanceMoves cleanedInput `shouldBe` danceMoves

      it "given dance example" $
        dance 5 danceMoves `shouldBe` "baedc"

    describe "Part 2" $

      it "given whole dance example" $
        wholeDance 5 danceMoves 2 `shouldBe` "ceadb"
