{-# LANGUAGE RecordWildCards #-}
module Day5 where

import Text.Megaparsec
import Data.Maybe
import Debug.Trace

import Util

day5 :: String -> Int
day5 input = minimum [f seed | seed <- pSeeds puzzle]
  where
  puzzle = myParse puzzleP input
  f = puzzleToFunction puzzle

day5_2 :: String -> Int
day5_2 input =
  minimum [f seed | seed <- allSeeds]
  where
  puzzle = myParse puzzleP input
  f = puzzleToFunction puzzle
  mkSeeds [] = []
  mkSeeds (start:len:rest) = [start..start+len] <> mkSeeds rest
  mkSeeds _ = error "uneven seeds"
  allSeeds = mkSeeds (pSeeds puzzle)

seedsP :: Parser [Int]
seedsP = chunk "seeds:" *> ws *> numP `sepBy` space

type Range = (Int, Int, Int)

data Section = Section
  { sName :: String
  , sRanges :: [Range]
  }
  deriving (Show, Eq)

data Puzzle = Puzzle
  { pSeeds :: [Int]
  , pSections :: [Section]
  }
  deriving (Show, Eq)

sectionP :: Parser Section
sectionP = do
  name <- kebabIdentifier <* ws <* chunk "map:" <* ws
  ranges <- many (rangeP <* nl)
  return $ Section
    { sName = name
    , sRanges = ranges
    }

rangeP :: Parser (Int, Int, Int)
rangeP = do
  dest <- numP
  ws
  src <- numP
  ws
  len <- numP
  return (dest, src, len)

puzzleP :: Parser Puzzle
puzzleP = do
  seeds <- seedsP <* ws
  sections <- sectionP `sepBy` ws
  eof
  return $ Puzzle
    { pSeeds = seeds
    , pSections = sections
    }

sectionToFunction :: Section -> Int -> Int
sectionToFunction Section{..} x = result
  -- trace (show x <> " " <> sName <> " " <> show result) result
  where
  rangeFs = map rangeToFunction sRanges
  mbResult = listToMaybe $ catMaybes [f x | f <- rangeFs]
  result = fromMaybe x mbResult

rangeToFunction :: Range -> Int -> Maybe Int
rangeToFunction (dst, src, len) x =
  if x >= src && x < src + len
  then Just (dst + x - src)
  else Nothing

puzzleToFunction :: Puzzle -> Int -> Int
puzzleToFunction Puzzle{..} x = foldr (.) id (map sectionToFunction $ reverse pSections) x
