import           Day07      (bottomProgramName, mkProgram, parseInputLines,
                             rebalancedWeight)
import           Test.Hspec

main :: IO ()
main = hspec $ do

  let programs = [
          mkProgram "pbga" 66 [],
          mkProgram "xhth" 57 [],
          mkProgram "ebii" 61 [],
          mkProgram "havc" 66 [],
          mkProgram "ktlj" 57 [],
          mkProgram "fwft" 72 ["ktlj", "cntj", "xhth"],
          mkProgram "qoyq" 66 [],
          mkProgram "padx" 45 ["pbga", "havc", "qoyq"],
          mkProgram "tknk" 41 ["ugml", "padx", "fwft"],
          mkProgram "jptl" 61 [],
          mkProgram "ugml" 68 ["gyxo", "ebii", "jptl"],
          mkProgram "gyxo" 61 [],
          mkProgram "cntj" 57 []
        ]

  describe "Day07 tests" $ do

    describe "Part 1" $ do

      it "can parse input file" $ do
        input <- readFile "Day07/test/input.txt"
        let inputLines = lines input
        parseInputLines inputLines `shouldBe` programs

      it "bottom program name" $
        bottomProgramName programs `shouldBe` Just "tknk"

    describe "Part 2" $

      it "rebalanced weight" $
        rebalancedWeight programs `shouldBe` Just ("ugml", 60)
