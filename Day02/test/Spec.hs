import           Day02      (checksum)
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "Day02 tests" $
    it "given spreadsheet example" $
      checksum xss `shouldBe` 18
      where
        xss =
          [
            [5, 1, 9, 5],
            [7, 5, 3],
            [2, 4, 6, 8]
          ]
