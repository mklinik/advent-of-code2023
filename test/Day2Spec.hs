module Day2Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class
import Text.Megaparsec
import qualified Data.Map as Map

import Day2
import Util

spec :: Spec
spec = describe "Day2" $ do
  it "parses tokens" $ do
    myParse colorP "red" `shouldBe` Red
    myParse numP "42" `shouldBe` 42
    myParse drawP "4 blue" `shouldBe` [(4, Blue)]
    myParse drawP "4 blue, 3 green" `shouldBe` [(4, Blue), (3, Green)]
    myParse gameP "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      `shouldBe` (1, [[(3, Blue), (4, Red)], [(1, Red), (2, Green), (6, Blue)], [(2, Green)]])
  
  it "determines possibility of a draw" $ do
    drawPossible [(3, Blue)] `shouldBe` True
    drawPossible [(3000, Blue)] `shouldBe` False

  it "determines possibility of the example games" $ do
    input <- liftIO $ readFile "day2example.txt"
    let exampleGames = myParse day2P input
    map gamePossible exampleGames `shouldBe`
      [(1, True)
      ,(2, True)
      ,(3, False)
      ,(4, False)
      ,(5, True)
      ]

  it "solves the example" $ do
    input <- liftIO $ readFile "day2example.txt"
    myParse day2P input `shouldSatisfy` (\x -> length x == 5)
    day2 input `shouldBe` 8

  it "solves part 1" $ do
    input <- liftIO $ readFile "day2.txt"
    day2 input `shouldBe` 2810

  it "determines minimum possible of a draw" $ do
    minimumPossible [(1, Red)] `shouldBe` Map.fromList [(Red, 1)]
  
  it "determines minimum possible of example Game 1" $ do
    let exampleGame1 = myParse gameP "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    minimumPossibleGame exampleGame1 `shouldBe`
      (1, Map.fromList [(Red, 4),(Green, 2),(Blue, 6)])
    gamePower exampleGame1 `shouldBe` 48

  it "solves the part 2 example" $ do
    input <- liftIO $ readFile "day2example.txt"
    day2_2 input `shouldBe` 2286

  it "solves part 2" $ do
    input <- liftIO $ readFile "day2.txt"
    day2_2 input `shouldBe` 69110
