import           Day02      (checksum1, checksum2)
import           Test.Hspec

xss1 =
  [
    [5, 1, 9, 5],
    [7, 5, 3],
    [2, 4, 6, 8]
  ]

xss2 =
  [
    [5, 9, 2, 8],
    [9, 4, 7, 3],
    [3, 8, 6, 5]
  ]

main :: IO ()
main = hspec $

  describe "Day02 tests" $ do

    describe "Part 1" $

      it "checksum1 given example" $
        checksum1 xss1 `shouldBe` 18

    describe "Part 2" $

      it "checksum2 given example" $
        checksum2 xss2 `shouldBe` 9
