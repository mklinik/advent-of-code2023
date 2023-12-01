module Day1 where

import Text.Megaparsec
import Data.List

import Util

day1 :: String -> Int
day1 input = sum $ map evalLine $ myParse day1P input

day1_2 :: String -> Int
day1_2 input = sum $ map evalLine $ myParse day1_2P input

data Tok = N Char | C Char
  deriving (Show, Eq)

tokenP :: Parser Tok
tokenP =
      C <$> oneOf ['a'..'z']
  <|> N <$> oneOf ['0'..'9']

token1_2P :: Parser Tok
token1_2P =
      N '1' <$ spelledNumber "one"
  <|> N '2' <$ spelledNumber "two"
  <|> N '3' <$ spelledNumber "three"
  <|> N '4' <$ spelledNumber "four"
  <|> N '5' <$ spelledNumber "five"
  <|> N '6' <$ spelledNumber "six"
  <|> N '7' <$ spelledNumber "seven"
  <|> N '8' <$ spelledNumber "eight"
  <|> N '9' <$ spelledNumber "nine"
  <|> C <$> oneOf ['a'..'z']
  <|> N <$> oneOf ['0'..'9']
  where
  spelledNumber word = lookAhead (chunk word) *> takeP (Just "spelled number") 1

day1P :: Parser [[Tok]]
day1P = many (many tokenP <* single '\n') <* eof

day1_2P :: Parser [[Tok]]
day1_2P = many ((many token1_2P) <* single '\n') <* eof

ns :: [Tok] -> String
ns toks = [n | N n <- toks]

evalLine :: [Tok] -> Int
evalLine toks = read $ [head foo] <> [head (reverse foo)]
  where
  foo = ns toks
