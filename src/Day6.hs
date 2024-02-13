{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Day6 where

import Text.Megaparsec
import Data.Maybe
import Debug.Trace

import Util

day6 :: String -> Int
day6 _input = 0

day5_2 :: String -> Int
day5_2 _input = 0

sectionP :: Parser Int
sectionP = numP

solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = (lower, upper)
  where
  sqr = sqrt $ (b*b) - (4*a*c)
  lower = ((-b) - sqr) / (2*a)
  upper = ((-b) + sqr) / (2*a)

raceBounds :: Int -> Int -> (Int, Int)
raceBounds t d = (adjustLower, adjustUpper)
  where
  (floor -> upper, ceiling -> lower) = solveQuadratic a b c
  a = -1
  b = fromIntegral t
  c = -(fromIntegral d)
  adjustUpper = if check t d upper == 0 then upper - 1 else upper
  adjustLower = if check t d lower == 0 then lower + 1 else lower

check :: Int -> Int -> Int -> Int
check t d p = p * (t - p) - d

solveRace :: Int -> Int -> Int
solveRace t d = 1 + upper - lower
  where
  (lower, upper) = raceBounds t d

solvePuzzle :: [(Int, Int)] -> Int
solvePuzzle input = product $ map (uncurry solveRace) input

{-

Given: integers time t, distance d
Wanted: integer p such that
  - p between 0 and t
  -   - p > 0
  -   - p < t
  - let q = t - p
  - p*(t - p) > d
There will be multiple solutions

p*(t - p) > d
pt - p^2 - d > 0
-p^2 + pt - d > 0
a = -p
b = t
c = -d

general formula for solving quadratic equations

x_12 = (-b +- sqrt (b^2 - 4ac)) / 2a

here

p_12 = (-(1+t) +- sqrt ((1+t)^2 - 4(-1)(-d))) / 2(-1)
     = t-1 +- sqrt ((1+t)^2 - 4d) / -2


-}
