module Main (main) where

import Day12
import Util

main :: IO ()
main = do
  input <- readFile "day12.txt"
  print $ time $ day12_2 input
