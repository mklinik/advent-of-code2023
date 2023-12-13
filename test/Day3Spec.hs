module Day3Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import Data.List

import Day3
import Util

spec :: Spec
spec = describe "Day3" $ do
  it "parses tokens" $ do
    myParse tokenP "123" `shouldBe` N (Number (1,1) 123 (1,4))
    myParse tokenP "." `shouldBe` Dot
    myParse tokenP "*" `shouldBe` S (Symbol (1,1) '*')

  it "parses the example" $ do
    input <- liftIO $ readFile "day3example.txt"
    myParse day3P input `shouldSatisfy` \r ->
      [ N $ Number (1,1) 467 (1,4)
      , Dot, Dot
      , N $ Number (1,6) 114 (1,9)
      , Dot, Dot, Ws
      , Dot, Dot, Dot
      , S $ Symbol (2, 4) '*'
      ]
      `isPrefixOf` r

  it "calculates adjacency" $ do
    isAdjacent (Symbol (2, 4) '*') (Number (1,1) 467 (1,4)) `shouldBe` True
    isAdjacent (Symbol (2, 4) '*') (Number (1,6) 114 (1,9)) `shouldBe` False

  it "solves the example" $ do
    input <- liftIO $ readFile "day3example.txt"
    day3 input `shouldBe` 4361

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day3.txt"
    day3 input `shouldBe` 498559

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day3example.txt"
    day3_2 input `shouldBe` 467835

  it "solves part 2" $ do
    input <- liftIO $ readFile "day3.txt"
    day3_2 input `shouldBe` 72246648
