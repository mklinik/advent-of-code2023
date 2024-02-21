{-# LANGUAGE RecordWildCards #-}
module Day8Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Data.Map as Map

import Day8
import Util

spec :: Spec
spec = describe "Day8" $ do
  it "parses a node" $ do
    let input = "BBB = (DDD, EEE)"
    myParse nodeP input `shouldBe` Node "BBB" "DDD" "EEE"

  it "parses the example" $ do
    input <- liftIO $ readFile "day8example.txt"
    myParse puzzleP input `shouldSatisfy` \Puzzle{..} ->
      length instructions == 3 && length network == 3

  it "solves the example" $ do
    input <- liftIO $ readFile "day8example.txt"
    day8 input `shouldBe` 6

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day8.txt"
    day8 input `shouldBe` 16579

  -- it "identifies an end state" $ do
    -- isEndState (map mkState ["AAZ", "ZZZ", "QUZ"]) `shouldBe` True
    -- isEndState (map mkState ["AAZ", "ZZZ", "QUX"]) `shouldBe` False

  it "parses the example 2" $ do
    input <- liftIO $ readFile "day8example2.txt"
    myParse puzzleP input `shouldSatisfy` \Puzzle{..} ->
      length instructions == 2 && length network == 8

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day8example2.txt"
    day8_2 input `shouldBe` 6
