{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Day9 where

import Text.Megaparsec
import Data.Maybe
import Debug.Trace
import Data.Char
import qualified Data.Map as Map
import Debug.Trace as Debug
import Data.List
import Control.DeepSeq
import qualified Data.Set as Set
import Data.Hashable
import qualified Data.IntMap as IntMap

import Util

day9 :: String -> Integer
day9 input = sum $ map extrapolateForward $ myParse puzzleP input

day9_2 :: String -> Integer
day9_2 input = sum $ map extrapolateBackward $ myParse puzzleP input

type Puzzle = [[Integer]]

lineP :: Parser [Integer]
lineP = numP `sepBy` space

puzzleP :: Parser Puzzle
puzzleP = do
  series <- many (lineP <* nl) <* eof
  return $ series

deltas :: [Integer] -> [Integer]
deltas [] = []
deltas [_] = []
deltas (x:y:rest) = y-x : deltas (y:rest)

allDeltas :: [Integer] -> [[Integer]]
allDeltas input
  | all (== 0) input = [input]
  | otherwise =
      let level1 = deltas input
      in input : allDeltas level1

reverseAll :: [[Integer]] -> [[Integer]]
reverseAll input = reverse $ map reverse input

extrapolateForward :: [Integer] -> Integer
extrapolateForward series = sum finals
  where
  finals = map head $ reverseAll $ allDeltas series

extrapolateBackward :: [Integer] -> Integer
extrapolateBackward series = go 0 initials
  where
  initials = reverse $ map head $ allDeltas series
  go x [] = x
  go x (n:rest) = go (n-x) rest
