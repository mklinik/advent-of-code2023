module Day12Spec where

import Test.Hspec
import Control.Monad.IO.Class
import qualified Data.MultiSet as MultiSet

import Day12
import Util

spec :: Spec
spec = fdescribe "day 12" $ do
  it "parses some tokens" $ do
    myParse conditionRecordP "???.###" `shouldBe` "???.###"
    myParse damageReportP "1,1,3" `shouldBe` [1,1,3]
    myParse rowP "?.# 1,1" `shouldBe` ("?.#", [1,1])

  it "solves the example" $ do
    input <- liftIO $ readFile "day12example.txt"
    day12 input `shouldBe` 21

  it "evals a state" $ do
    mkNFA [1] `shouldBe` [Dot,Hash 1]
    step (mkNFA [1]) '.' `shouldBe` [[Dot,Hash 1]]
    step (mkNFA [1]) '#' `shouldBe` [[Hash 1]]

  it "runs an automaton" $ do
    MultiSet.occur [Hash 1] (process (mkNFA [1]) "..#") `shouldBe` 1
    evalRow (myParse rowP ".#. 1") `shouldBe` 1
    evalRow (myParse rowP ".#. 1") `shouldBe` 1
    evalRow (myParse rowP "#.#.### 1,1,3") `shouldBe` 1
    evalRow (myParse rowP "??? 1") `shouldBe` 3
    evalRow (myParse rowP ".??..??...?##. 1,1,3") `shouldBe` 4
    evalRow (myParse rowP "?#?#?#?#?#?#?#? 1,3,1,6") `shouldBe` 1
    evalRow (myParse rowP "????.######..#####. 1,6,5") `shouldBe` 4
    evalRow (myParse rowP "?###???????? 3,2,1") `shouldBe` 10

  it "evaluates the unfolded examles" $ do
    evalRow (unfoldRow $ myParse rowP "???.### 1,1,3") `shouldBe` 1
    evalRow (unfoldRow $ myParse rowP ".??..??...?##. 1,1,3") `shouldBe` 16384
    evalRow (unfoldRow $ myParse rowP "?#?#?#?#?#?#?#? 1,3,1,6") `shouldBe` 1
    evalRow (unfoldRow $ myParse rowP "????.#...#... 4,1,1") `shouldBe` 16
    evalRow (unfoldRow $ myParse rowP "????.######..#####. 1,6,5") `shouldBe` 2500
    evalRow (unfoldRow $ myParse rowP "?###???????? 3,2,1") `shouldBe` 506250

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day12example.txt"
    day12_2 input `shouldBe` 525152

  it "solves part 2" $ do
    pending
    input <- liftIO $ readFile "day12.txt"
    day12_2 input `shouldBe` 50338344809230
