module Day1Spec (spec) where

import Test.Hspec
import Control.Monad.IO.Class

import Day1

spec :: Spec
spec = describe "Day1" $ do
  it "foobars" $ do
    input <- liftIO $ readFile "day1example.txt"
    day1 input `shouldBe` 0
