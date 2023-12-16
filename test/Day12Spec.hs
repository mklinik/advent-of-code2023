module Day12Spec where

import Test.Hspec
import Control.Monad.IO.Class

import Day12
import Util

spec :: Spec
spec = describe "day 12" $ do
  it "parses some tokens" $ do
    myParse conditionRecordP "???.###" `shouldBe` [U, U, U, O, D, D, D]
    myParse damageReportP "1,1,3" `shouldBe` [1,1,3]
    myParse rowP "?.# 1,1" `shouldBe` ([U,O,D], [1,1])

  describe "mkReport" $ do
    it "generates correct reports" $ do
      myParse rowP "#.#.### 1,1,3" `shouldSatisfy` (\(x,y) -> mkReport x == y)
      myParse rowP ".#.###.#.###### 1,3,1,6" `shouldSatisfy` (\(x,y) -> mkReport x == y)

  describe "fillRecord" $ do
    it "returns a ConditionRecord without unknowns unchanged" $ do
      fillRecord [] [O, D, D, O] `shouldBe` [O, D, D, O]
    it "fills the unknows with the given values" $ do
      fillRecord [D, O] [U, U] `shouldBe` [D, O]

  it "solves some example rows" $ do
    solveRow (myParse rowP "???.### 1,1,3") `shouldBe` 1
    solveRow (myParse rowP ".??..??...?##. 1,1,3") `shouldBe` 4
    solveRow (myParse rowP "?###???????? 3,2,1") `shouldBe` 10

  it "solves the example" $ do
    input <- liftIO $ readFile "day12example.txt"
    day12 input `shouldBe` 21
