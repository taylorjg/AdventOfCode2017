import           Day18
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day18 tests" $ do

    describe "Part 1" $ do

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

      it "can parse input" $ do
        input <- readFile "Day18/test/input1.txt"
        parseInput input `shouldBe` instructions

      it "given example" $
        runProgram instructions `shouldBe` Just 4

    describe "Part 2" $

      it "given example" $ do
        input <- readFile "Day18/test/input2.txt"
        let instructions = parseInput input
        runTwoPrograms instructions `shouldBe` 3
