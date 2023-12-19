module Day3 where

import Text.Megaparsec
import Data.Char

import Util

day3 :: String -> Int
day3 input = sum partNumbers
  where
  allTokens = myParse day3P input
  symbols = [s | S s <- allTokens]
  numbers = [n | N n <- allTokens]
  partNumbers =
    [ value
    | n@(Number _ value _) <- numbers
    , any (flip isAdjacent n) symbols
    ]

day3_2 :: String -> Int
day3_2 input = sum [n1 * n2 | (n1, n2) <- gears]
  where
  allTokens = myParse day3P input
  stars = [s | S s@(Symbol _ '*') <- allTokens]
  numbers = [n | N n <- allTokens]
  gears =
    [ (n1, n2)
    | s <- stars
    , [Number _ n1 _, Number _ n2 _] <- [filter (isAdjacent s) numbers]
    ]

data Number = Number MySourcePos Int MySourcePos
  deriving (Show, Eq)

data Symbol = Symbol MySourcePos Char
  deriving (Show, Eq)

data Tok
  = N Number
  | Dot
  | S Symbol
  | Ws
  deriving (Show, Eq)

numberP :: Parser Number
numberP = Number <$> myPosP <*> numP <*> myPosP

symbolP :: Parser Symbol
symbolP = Symbol <$> myPosP <*> satisfy (not . isSpace)

tokenP :: Parser Tok
tokenP =
      Dot <$ single '.'
  <|> N <$> numberP
  <|> S <$> symbolP
  <|> Ws <$ ws

day3P :: Parser [Tok]
day3P = many tokenP

isAdjacent :: Symbol -> Number -> Bool
isAdjacent (Symbol (sy, sx) _) (Number (ny, nx1) _ (_, nx2)) = yAdjacent && xAdjacent
  where
  yAdjacent = abs (sy - ny) <= 1
  xAdjacent = sx >= nx1 - 1 && sx <= nx2
