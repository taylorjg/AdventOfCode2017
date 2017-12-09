import           Day09
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Day09 tests" $ do

    describe "Part 1" $ do

      describe "parse pieces of garbage" $ do

        it "<>" $
          parseGarbage "<>" `shouldBe` 2

        it "<random characters>" $
          parseGarbage "<random characters>" `shouldBe` 19

        it "<<<<>" $
          parseGarbage "<<<<>" `shouldBe` 5

        it "<{!>}>" $
          parseGarbage "<{!>}>" `shouldBe` 6

        it "<!!>" $
          parseGarbage "<!!>" `shouldBe` 4

        it "<!!!>>" $
          parseGarbage "<!!!>>" `shouldBe` 6

        it "<{o\"i!a,<{i<a>" $
          parseGarbage "<{o\"i!a,<{i<a>" `shouldBe` 14

      describe "parse nested groups" $ do

        it "{}" $
          parseGroups "{}" `shouldBe` 1

        it "{{{}}}" $
          parseGroups "{{{}}}" `shouldBe` 3

        it "{{},{}}" $
          parseGroups "{{},{}}" `shouldBe` 3

        it "{{{},{},{{}}}}" $
          parseGroups "{{{},{},{{}}}}" `shouldBe` 6

        it "{<{},{},{{}}>}" $
          parseGroups "{<{},{},{{}}>}" `shouldBe` 1

        it "{<a>,<a>,<a>,<a>}" $
          parseGroups "{<a>,<a>,<a>,<a>}" `shouldBe` 1

        it "{{<a>},{<a>},{<a>},{<a>}}" $
          parseGroups "{{<a>},{<a>},{<a>},{<a>}}" `shouldBe` 5

        it "{{<!>},{<!>},{<!>},{<a>}}" $
          parseGroups "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` 2

      describe "score nested groups" $ do

        it "{}" $
          scoreGroups "{}" `shouldBe` 1

        it "{{{}}}" $
          scoreGroups "{{{}}}" `shouldBe` 6

        it "{{},{}}" $
          scoreGroups "{{},{}}" `shouldBe` 5

        it "{{{},{},{{}}}}" $
          scoreGroups "{{{},{},{{}}}}" `shouldBe` 16

        it "{<a>,<a>,<a>,<a>}" $
          scoreGroups "{<a>,<a>,<a>,<a>}" `shouldBe` 1

        it "{{<ab>},{<ab>},{<ab>},{<ab>}}" $
          scoreGroups "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9

        it "{{<!!>},{<!!>},{<!!>},{<!!>}}" $
          scoreGroups "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9

        it "{{<a!>},{<a!>},{<a!>},{<ab>}}" $
          scoreGroups "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3

    describe "Part 2" $ do

      it "<>" $
        lengthOfGarbageContent "<>" `shouldBe` 0

      it "<random characters>" $
        lengthOfGarbageContent "<random characters>" `shouldBe` 17

      it "<<<<>" $
        lengthOfGarbageContent "<<<<>" `shouldBe` 3

      it "<{!>}>" $
        lengthOfGarbageContent "<{!>}>" `shouldBe` 2

      it "<!!>" $
        lengthOfGarbageContent "<!!>" `shouldBe` 0

      it "<!!!>>" $
        lengthOfGarbageContent "<!!!>>" `shouldBe` 0

      it "<{o\"i!a,<{i<a>" $
        lengthOfGarbageContent "<{o\"i!a,<{i<a>" `shouldBe` 10
