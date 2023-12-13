module Day4 where

import Text.Megaparsec
import Data.Char
import qualified Data.Set as Set
import Data.Maybe

import Util

day4 :: String -> Integer
day4 input = sum $ map evalCard $ myParse day4P input

day4_2 :: String -> Int
day4_2 input = length $ evalPuzzle2 $ myParse day4P input

type Card = (Int, ([Integer], [Integer]))

cardP :: Parser Card
cardP = do
  _ <- chunk "Card" <* ws
  cardNo <- numP <* single ':' <* ws
  winningNumbers <- many (numP <* ws)
  _ <- single '|' <* ws
  yourNumbers <- many (numP <* optional ws)
  return (cardNo, (winningNumbers, yourNumbers))

day4P :: Parser [Card]
day4P = many cardP

evalCard :: Card -> Integer
evalCard (_, (winningNumbers, yourNumbers)) = if s == 0 then 0 else 2 ^ (s - 1)
  where
  yourWinningNumbers = Set.intersection (Set.fromList winningNumbers) (Set.fromList yourNumbers)
  s = Set.size yourWinningNumbers

evalCard2 :: [Card] -> Card -> [Card]
evalCard2 original (cardNo, (winningNumbers, yourNumbers)) =
  [ (c, x)
  | c <- cardsWon
  , Just x <- [lookup c original]
  ]
  where
  yourWinningNumbers = Set.intersection (Set.fromList winningNumbers) (Set.fromList yourNumbers)
  s = Set.size yourWinningNumbers
  cardsWon = if s == 0 then [] else [cardNo+1..cardNo+s]

evalPuzzle2 :: [Card] -> [Card]
evalPuzzle2 original = go original []
  where
  go [] acc = acc
  go (c:cs) acc = go (cs <> evalCard2 original c) (c:acc)
