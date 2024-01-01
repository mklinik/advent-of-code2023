module Main (main) where

import Day19
import Util

main :: IO ()
main = do
  input <- readFile "day19.txt"
  print $ time $ day19_2 input
