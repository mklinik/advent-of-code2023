module Main (main) where

import Day9
import Util

main :: IO ()
main = do
  input <- readFile "day9.txt"
  print $ day9_2 input
  -- let input = myParse lineP "10 13 16 21 30 45"
  -- mapM_ print $ map head $ allDeltas input
  -- print $ extrapolateBackward input
