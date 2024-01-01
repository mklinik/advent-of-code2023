module Day19Spec where

import Test.Hspec
import qualified Data.Map as Map
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.Set (Set)

import Day19
import Util

spec :: Spec
spec = fdescribe "day19" $ do

  it "parses a part" $ do
    myParse partP "{x=787,m=2655,a=1222,s=2876}" `shouldBe` Part 787 2655 1222 2876

  it "parses a workflow" $ do
    let
      w = myParse workflowP "px{a<2006:qkq,m>2090:A,rfg}"
      p1 = myParse partP "{x=0,m=2655,a=1222,s=2876}"
      p2 = myParse partP "{x=9999,m=9999,a=9999,s=2876}"
      p3 = myParse partP "{x=9999,m=0,a=9999,s=2876}"
    name w `shouldBe` "px"
    f w p1 `shouldBe` SendTo "qkq"
    f w p2 `shouldBe` Accept
    f w p3 `shouldBe` SendTo "rfg"

  it "parses the example" $ do
    input <- liftIO $ readFile "day19example.txt"
    let puzzle = myParse puzzleP input
    length (workflows puzzle) `shouldBe` 11
    length (parts puzzle) `shouldBe` 5

  it "processes some parts from the example" $ do
    input <- liftIO $ readFile "day19example.txt"
    let
      puzzle = myParse puzzleP input
      part1 = myParse partP "{x=787,m=2655,a=1222,s=2876}"
      part2 = myParse partP "{x=1679,m=44,a=2067,s=496}"
    process puzzle part1 `shouldBe` Just part1
    process puzzle part2 `shouldBe` Nothing

  it "solves the example" $ do
    input <- liftIO $ readFile "day19example.txt"
    day19 input `shouldBe` 19114

  it "solves the puzzle" $ do
    input <- liftIO $ readFile "day19.txt"
    day19 input `shouldBe` 323625

  it "symbolically evaluates a condition" $ do
    symEvalCondition 'x' '>' 10 defaultSymPart `shouldSatisfy` \(thenPart, elsePart) ->
      spx thenPart == (11, 4000)
      && spx elsePart == (1, 10)
    symEvalCondition 'm' '<' 50 defaultSymPart `shouldSatisfy` \(thenPart, elsePart) ->
      spm thenPart == (1, 49)
      && spm elsePart == (50, 4000)

  it "symbolically evaluates a workflow" $ do
    let wf = myParse symWorkflowP "in{x>10:A,m<50:R,foo}"
    Set.fromList (symEvalWorkflow (swRules wf) defaultSymPart) `shouldBe` Set.fromList
      [ (SymPart { spx = (11, 4000) , spm = ( 1, 4000) , spa = (1, 4000) , sps = (1, 4000) }, Accept)
      , (SymPart { spx = ( 1,   10) , spm = ( 1,   49) , spa = (1, 4000) , sps = (1, 4000) }, Reject)
      , (SymPart { spx = ( 1,   10) , spm = (50, 4000) , spa = (1, 4000) , sps = (1, 4000) }, SendTo "foo")
      ]

  it "solves the example part 2" $ do
    input <- liftIO $ readFile "day19example.txt"
    day19_2 input `shouldBe` 167409079868000

  it "solves part 2" $ do
    input <- liftIO $ readFile "day19.txt"
    day19_2 input `shouldBe` 127447746739409
