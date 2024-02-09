module Main (main) where

import Day4
import Util

main :: IO ()
main = do
  input <- readFile "day4.txt"
  print $ time $ day4_2 input
