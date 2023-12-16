module Main (main) where

import Day12

main :: IO ()
main = do
  input <- readFile "day12.txt"
  print $ day12 input
