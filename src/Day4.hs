module Day4 where

import Text.Megaparsec
import Data.Char
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)

import Util

import Debug.Trace as Debug

day4 :: String -> Integer
day4 input = sum $ map evalCard $ myParse day4P input

day4_2 :: String -> Int
day4_2 input = evalCard2 cards (map fst table)
  where
  table = map winnings $ myParse day4P input
  cards = mkCards table

type Card = (Int, ([Integer], [Integer]))
type Card2 = (Int, [Int])

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

winnings :: Card -> Card2
winnings (cardNo, (winningNumbers, yourNumbers)) = (cardNo, wonCards)
  where
  wonCards = [cardNo+1 .. (cardNo+s)]
  yourWinningNumbers = Set.intersection (Set.fromList winningNumbers) (Set.fromList yourNumbers)
  s = Set.size yourWinningNumbers

type Cards = Map Int [Int]

mkCards :: [Card2] -> Cards
mkCards = Map.fromList

evalCard2 :: Cards -> [Int] -> Int
evalCard2 cards cardsWon = length cardsWon + sum recurse
  where
  recurse :: [Int]
  recurse =
    [ evalCard2 cards $ fromMaybe [] $ Map.lookup wonCard cards
    | wonCard <- cardsWon
    ]
