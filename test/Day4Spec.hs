module Day4Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import Data.List

import Day4
import Util

spec :: Spec
spec = fdescribe "Day4" $ do
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
    let original = myParse day4P input
    map fst (evalCard2 original (head original)) `shouldBe` [2, 3, 4, 5]

  it "solves the example 2" $ do
    input <- liftIO $ readFile "day4example.txt"
    day4_2 input `shouldBe` 30

  it "solves part 2" $ do
    pendingWith "let's not do that here"
    input <- liftIO $ readFile "day4.txt"
    day4_2 input `shouldBe` 30
