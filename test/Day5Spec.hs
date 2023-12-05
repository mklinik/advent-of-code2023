{-# LANGUAGE RecordWildCards #-}
module Day5Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Data.Map as Map

import Day5
import Util

spec :: Spec
spec = describe "Day5" $ do
  it "parses tokens" $ do
    myParse seedsP "seeds: 79 14 55 13" `shouldBe` [79, 14, 55, 13]
    myParse rangeP "1 2 3" `shouldBe` (1, 2, 3)

  it "parses a section" $ do
    let input = unlines
          [ "seed-to-soil map:"
          , "50 98 2"
          , "52 50 48"
          ]
    myParse sectionP input `shouldBe` Section "seed-to-soil"
      [ (50, 98, 2)
      , (52, 50, 48)
      ]

  it "parses the example" $ do
    input <- liftIO $ readFile "day5example.txt"
    myParse puzzleP input `shouldSatisfy` \Puzzle{..} ->
      length pSeeds == 4 && length pSections == 7

  it "applies a range" $ do
    rangeToFunction (50, 98, 2) 97 `shouldBe` Nothing
    rangeToFunction (50, 98, 2) 98 `shouldBe` (Just 50)
    rangeToFunction (50, 98, 2) 99 `shouldBe` (Just 51)
    rangeToFunction (50, 98, 2) 100 `shouldBe` Nothing

  it "applies a section" $ do
    let input = unlines
          [ "seed-to-soil map:"
          , "50 98 2"
          , "52 50 48"
          ]
        section = myParse sectionP input
        f = sectionToFunction section
    f 0 `shouldBe` 0
    f 1 `shouldBe` 1
    f 48 `shouldBe` 48
    f 49 `shouldBe` 49
    f 50 `shouldBe` 52
    f 51 `shouldBe` 53
    f 96 `shouldBe` 98
    f 97 `shouldBe` 99
    f 98 `shouldBe` 50
    f 99 `shouldBe` 51
    f 100 `shouldBe` 100

  it "applies the example" $ do
    input <- liftIO $ readFile "day5example.txt"
    let puzzle = myParse puzzleP input
        f = puzzleToFunction puzzle
    f 79 `shouldBe` 82

  it "solves the example" $ do
    input <- liftIO $ readFile "day5example.txt"
    day5 input `shouldBe` 35

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day5.txt"
    day5 input `shouldBe` 173706076

  it "solves part 2 of the puzzle" $ do
    pendingWith "let's not do that here, takes too long"
    input <- liftIO $ readFile "day5.txt"
    day5_2 input `shouldBe` 11611182
