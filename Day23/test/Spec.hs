import           Day23
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day23 tests" $

    describe "Part 1" $ do

      let instructions = [
                Set 'b' (Val 65),
                Set 'c' (Reg 'b'),
                Jnz (Reg 'a') (Val 2),
                Jnz (Val 1) (Val 5),
                Mul 'b' (Val 100),
                Sub 'b' (Val (-100000))
              ]

      it "can parse input" $ do
        input <- readFile "Day23/test/input.txt"
        parseInput input `shouldBe` instructions

      it "runProgramWithDebugOn" $ do
        input <- readFile "Day23/src/input.txt"
        runProgramWithDebugOn (parseInput input) `shouldBe` 3969
