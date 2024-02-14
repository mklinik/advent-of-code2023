module Day7Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import Data.List

import Day7
import Util

spec :: Spec
spec = describe "Day7" $ do
  it "parses a hand" $ do
    myParse handP "32T3K 765" `shouldBe` Hand
      { cards = "32T3K"
      , bid = 765
      , handType = OnePair
      , cardNums = [3, 2, 10, 3, 13]
      }

  it "determines the numerical value of a card" $ do
    numerical 'T' `shouldBe` 10
    numerical '2' `shouldBe` 2
    numerical '9' `shouldBe` 9

  it "determines the hand type of some hands" $ do
    mkHandType "AAAAA" `shouldBe` Five
    mkHandType "AA8AA" `shouldBe` Four
    mkHandType "23332" `shouldBe` FullHouse
    mkHandType "TTT98" `shouldBe` Three
    mkHandType "23432" `shouldBe` TwoPair
    mkHandType "A23A4" `shouldBe` OnePair
    mkHandType "23456" `shouldBe` HighCard

  it "orders hands by strength" $ do
    let
      hand1 = myParse handP "33332 0"
      hand2 = myParse handP "2AAAA 0"
    hand1 > hand2


  it "parses the example" $ do
    input <- liftIO $ readFile "day7example.txt"
    myParse day7P input `shouldSatisfy` \r ->
      length r == 5

  it "solves the example" $ do
    input <- liftIO $ readFile "day7example.txt"
    day7 input `shouldBe` 6440

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day7.txt"
    day7 input `shouldBe` 248179786

  it "determines the hand type with jokers" $ do
    let
      hand = mkHand2 $ myParse handP "QJJQ2 0"
    handType hand `shouldBe` Four

  it "compares hands with jokers" $ do
    let
      hand1 = mkHand2 $ myParse handP "JKKK2 0"
      hand2 = mkHand2 $ myParse handP "QQQQ2 0"
    hand1 < hand2 `shouldBe` True

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day7example.txt"
    day7_2 input `shouldBe` 5905

  it "solves the puzzle part 2" $ do
    input <- liftIO $ readFile "day7.txt"
    day7_2 input `shouldBe` 247885995
