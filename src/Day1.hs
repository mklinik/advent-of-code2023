module Day1 where

import Text.Megaparsec

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
      N '1' <$ spelledDigit "one"
  <|> N '2' <$ spelledDigit "two"
  <|> N '3' <$ spelledDigit "three"
  <|> N '4' <$ spelledDigit "four"
  <|> N '5' <$ spelledDigit "five"
  <|> N '6' <$ spelledDigit "six"
  <|> N '7' <$ spelledDigit "seven"
  <|> N '8' <$ spelledDigit "eight"
  <|> N '9' <$ spelledDigit "nine"
  <|> C <$> oneOf ['a'..'z']
  <|> N <$> oneOf ['0'..'9']
  where
  spelledDigit word = lookAhead (chunk word) *> takeP (Just "spelled digit") 1

ns :: [Tok] -> String
ns toks = [n | N n <- toks]

evalLine :: [Tok] -> Int
evalLine toks = read [head digits, last digits]
  where
  digits = ns toks

day1P :: Parser [[Tok]]
day1P = many (many tokenP <* single '\n') <* eof

day1_2P :: Parser [[Tok]]
day1_2P = many ((many token1_2P) <* single '\n') <* eof
