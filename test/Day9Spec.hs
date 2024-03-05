{-# LANGUAGE RecordWildCards #-}
module Day9Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Data.Map as Map

import Day9
import Util

spec :: Spec
spec = describe "Day9" $ do
  it "parses a line" $ do
    let input = "0 3 6 9 12 15"
    myParse lineP input `shouldBe` [0, 3, 6, 9, 12, 15]

  it "parses the example" $ do
    input <- liftIO $ readFile "day9example.txt"
    myParse puzzleP input `shouldSatisfy` \series ->
      length series == 3

  describe "deltas" $ do

    it "calculates deltas as described" $ do
      let level1 = deltas [0, 3, 6, 9, 12, 15]
          level2 = deltas level1
      level1 `shouldBe` [3, 3, 3, 3, 3]
      level2 `shouldBe` [0, 0, 0, 0]

    it "calculates allDeltas" $ do
      allDeltas [0, 3, 6, 9, 12, 15] `shouldBe`
        [ [0, 3, 6, 9, 12, 15]
        , [3, 3, 3, 3, 3]
        , [0, 0, 0, 0]
        ]

    it "reverses correctly" $ do
      reverseAll
        [ [1, 2, 3]
        , [4, 5, 6]
        ]
        `shouldBe`
        [ [6, 5, 4]
        , [3, 2, 1]
        ]
  
  it "extrapolateBackward first example series correctly" $ do
    extrapolateBackward [0, 3, 6, 9, 12, 15] `shouldBe` (-3)

  fit "extrapolateBackward third example series correctly" $ do
    extrapolateBackward [10, 13, 16, 21, 30, 45] `shouldBe` 5

  it "solves all series of the example" $ do
    input <- liftIO $ readFile "day9example.txt"
    let series = myParse puzzleP input
    map extrapolateForward series `shouldBe`
      [18, 28, 68]

  it "solves all series backwards of the example" $ do
    input <- liftIO $ readFile "day9example.txt"
    let series = myParse puzzleP input
    map extrapolateBackward series `shouldBe`
      [-3, 0, 5]

  it "solves the example" $ do
    input <- liftIO $ readFile "day9example.txt"
    day9 input `shouldBe` 114

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day9example.txt"
    day9_2 input `shouldBe` 2
