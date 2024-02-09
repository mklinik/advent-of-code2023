module Day4Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import Data.List

import Day4
import Util

spec :: Spec
spec = describe "Day4" $ do
  it "parses a card" $ do
    myParse cardP "Card 1: 1 2 | 3 4" `shouldBe` (1, ([1, 2],[3, 4]))

  it "evaluates a card" $ do
    evalCard (myParse cardP "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") `shouldBe` 8

  it "parses the example" $ do
    input <- liftIO $ readFile "day4example.txt"
    myParse day4P input `shouldSatisfy` \r ->
      length r == 6

  it "solves the example" $ do
    input <- liftIO $ readFile "day4example.txt"
    day4 input `shouldBe` 13

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day4.txt"
    day4 input `shouldBe` 25651

  it "eval2 a card" $ do
    input <- liftIO $ readFile "day4example.txt"
    let card1:card2:card3:card4:card5:card6:_ = myParse day4P input
    winnings card1 `shouldBe` (1, [2, 3, 4, 5])
    winnings card2 `shouldBe` (2, [3, 4])
    winnings card6 `shouldBe` (6, [])

  it "solves the example 2" $ do
    input <- liftIO $ readFile "day4example.txt"
    day4_2 input `shouldBe` 30
