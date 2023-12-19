module Main (main) where

import Day16
import Util

main :: IO ()
main = do
  input <- readFile "day16.txt"
  print $ time $ day16_2 input
