module Day1Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec

import Day1
import Util

spec :: Spec
spec = describe "Day1" $ do
  it "parses tokens" $ do
    myParse (many tokenP) "a1b2" `shouldBe` [C 'a', N '1', C 'b', N '2']
    myParse token1_2P "seven" `shouldBe` N '7'
    ns (myParse (many token1_2P) "twoone") `shouldBe` "21"
    ns (myParse (many token1_2P) "sevenone") `shouldBe` "71"
    ns (myParse (many token1_2P) "two1nine") `shouldBe` "219"
    ns (myParse (many token1_2P) "eightwothree") `shouldBe` "823"
    ns (myParse (many token1_2P) "abcone2threexyz") `shouldBe` "123"
    ns (myParse (many token1_2P) "xtwone3four") `shouldBe` "2134"
    ns (myParse (many token1_2P) "4nineeightseven2") `shouldBe` "49872"
    ns (myParse (many token1_2P) "zoneight234") `shouldBe` "18234"
    ns (myParse (many token1_2P) "7pqrstsixteen") `shouldBe` "76"
    ns (myParse (many token1_2P) "1sevenseven7ld") `shouldBe` "1777"

  it "evaluates a line" $ do
    evalLine (myParse (many tokenP) "a1b2c3d") `shouldBe` 13

  it "parses the example input" $ do
    input <- liftIO $ readFile "day1example.txt"
    myParse day1P input `shouldSatisfy` (\x -> length x == 4)

  it "solves the example" $ do
    input <- liftIO $ readFile "day1example.txt"
    day1 input `shouldBe` 142

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day1.txt"
    day1 input `shouldBe` 54632

  it "solves the second example" $ do
    input <- liftIO $ readFile "day1_2example.txt"
    myParse day1_2P input `shouldSatisfy` (\x -> length x == 7)
    day1_2 input `shouldBe` 281

  it "solves the second puzzle" $ do
    input <- liftIO $ readFile "day1.txt"
    myParse day1_2P input `shouldSatisfy` (\x -> length x == 1000)
    day1_2 input `shouldBe` 54019
