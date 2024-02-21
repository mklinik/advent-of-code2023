module Main (main) where

import Day8
import Util
import Data.List

main :: IO ()
main = do
  input <- readFile "day8.txt"
  let puzzle@Puzzle{..} = myParse puzzleP input
      startState = [name | Node{..} <- network, "A" `isSuffixOf` name]
  print startState
  -- print $ singleLoop puzzle "LJA"
  print $ map (time . singleLoop puzzle) startState
  -- print $ time $ day8 input
