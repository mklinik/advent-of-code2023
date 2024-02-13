{-# LANGUAGE RecordWildCards #-}
module Day6Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Data.Map as Map

import Day6
import Util

spec :: Spec
spec = describe "Day6" $ do
  it "solves the three example races" $ do
    solveQuadratic 5 1 (-4) `shouldBe` (-1, 0.8)
    solveQuadratic 2 (-4) 2 `shouldBe` (1, 1)
    solveQuadratic 7 (-14) 0 `shouldBe` (0, 2)

    raceBounds 7 9 `shouldBe` (2, 5)
    raceBounds 15 40 `shouldBe` (4, 11)
    raceBounds 30 200 `shouldBe` (11, 19)

    solveRace 7 9 `shouldBe` 4
    solveRace 15 40 `shouldBe` 8
    solveRace 30 200 `shouldBe` 9

  it "solves the example" $ do
    solvePuzzle
      [ (7, 9)
      , (15, 40)
      , (30, 200)
      ]
      `shouldBe` 288

  it "solves the puzzle" $ do
    solvePuzzle
      [ (62, 644)
      , (73, 1023)
      , (75, 1240)
      , (65, 1023)
      ]
      `shouldBe` 393120

  it "solves the puzzle part 2" $ do
    solveRace (62737565) (644102312401023) `shouldBe` 36872656
