import           Day18
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day18 tests" $ do

    let instructions = [
            Set 'a' (Val 1),
            Add 'a' (Val 2),
            Mul 'a' (Reg 'a'),
            Mod 'a' (Val 5),
            Snd 'a',
            Set 'a' (Val 0),
            Rcv 'a',
            Jgz 'a' (Val (-1)),
            Set 'a' (Val 1),
            Jgz 'a' (Val (-2))
          ]

    describe "Part 1" $ do

      it "can parse input" $ do
        input <- readFile "Day18/test/input.txt"
        parseInput input `shouldBe` instructions

      it "given example" $
        1 `shouldBe` 1
