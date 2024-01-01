module Day16Spec where

import Test.Hspec
import qualified Data.Map as Map
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.Set (Set)

import Day16
import Util

spec :: Spec
spec = describe "day16" $ do
  it "parses the example board" $ do
    input <- liftIO $ readFile "day16example.txt"
    let board = myParse boardP input
    Map.lookup (1,1) board `shouldBe` Just '.'
    Map.lookup (2,1) board `shouldBe` Just '|'
    Map.lookup (2,1) board `shouldBe` Just '|'
    Map.lookup (3,2) board `shouldBe` Just '-'
    Map.lookup (10,10) board `shouldBe` Just '.'

  it "takes some steps on the example board" $ do
    input <- liftIO $ readFile "day16example.txt"
    let
      board = myParse boardP input
    let
      step0 = Set.singleton (Beam (1,1) R)
      step1 = step board step0
      step2 = step board step1
    step1 `shouldBe` Set.fromList [Beam (2,1) R]
    step2 `shouldBe` Set.fromList [Beam (2,2) D]

  it "solves the example" $ do
    input <- liftIO $ readFile "day16example.txt"
    execute (myParse boardP input) (Beam (1,1) R) `shouldBe` 46

  it "calculates the right board dimensions of the example" $ do
    input <- liftIO $ readFile "day16example.txt"
    let
      board = myParse boardP input
    boardDimensions board `shouldBe` (10, 10)

  it "calculates the right board dimensions of the input" $ do
    input <- liftIO $ readFile "day16.txt"
    let
      board = myParse boardP input
    boardDimensions board `shouldBe` (110, 110)

  it "calculates the correct starting positions" $ do
    board <- liftIO $ myParse boardP <$> readFile "day16example.txt"
    possibleStarts board `shouldSatisfy` \l -> length l == 40
    boardReal <- liftIO $ myParse boardP <$> readFile "day16.txt"
    possibleStarts boardReal `shouldSatisfy` \l -> length l == 440

  it "solves part 2 example" $ do
    input <- liftIO $ readFile "day16example.txt"
    day16_2 input `shouldBe` 51
