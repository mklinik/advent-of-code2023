module Main (main) where

import Day5

main :: IO ()
main = do
  input <- readFile "day5.txt"
  print $ day5_2 input
  -- input <- readFile "day4.txt"
  -- print $ day4_2 input
