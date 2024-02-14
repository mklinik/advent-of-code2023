{-# LANGUAGE RecordWildCards #-}
module Day7 where

import Text.Megaparsec
import Data.Char
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import Util

import Debug.Trace as Debug

day7 :: String -> Integer
day7 input = sum $ (\x -> [bid hand * rank | (rank, hand) <- x]) $ zip [1..] $ sort $ myParse day7P input

day7_2 :: String -> Integer
day7_2 input = sum $ (\x -> [bid hand * rank | (rank, hand) <- x]) $ zip [1..] $ sort $ map mkHand2 $ myParse day7P input

data Hand = Hand
  { cards :: [Char]
  , bid :: Integer
  , handType :: HandType
  , cardNums :: [Int]
  }
  deriving (Show, Eq)

instance Ord Hand where
  left <= right = if handType left == handType right
    then cardNums left <= cardNums right
    else handType left <= handType right 

handP :: Parser Hand
handP = do
  cards <- takeWhile1P (Just "hand") (flip elem "AKQJT98765432") <* ws
  bid <- numP
  return $ mkHand cards bid

mkHand cards bid = Hand
  { cards = cards
  , bid = bid
  , handType = mkHandType cards
  , cardNums = map numerical cards
  }

day7P :: Parser [Hand]
day7P = many (handP <* nl)

numerical :: Char -> Int
numerical 'A' = 14 
numerical 'K' = 13 
numerical 'Q' = 12 
numerical 'J' = 11 
numerical 'T' = 10 
numerical c = ord c - ord '0'

data HandType = HighCard | OnePair | TwoPair | Three | FullHouse | Four | Five
  deriving (Eq, Show, Enum, Ord)

mkHandType :: [Char] -> HandType
mkHandType cards = evalEqualGroups $ reverse $ sort $ map length $ group $ sort cards

evalEqualGroups :: [Int] -> HandType
evalEqualGroups grouping = case grouping of
  [5] -> Five
  [4, 1] -> Four
  [3, 2] -> FullHouse
  [3, 1, 1] -> Three
  [2, 2, 1] -> TwoPair
  [2, 1, 1, 1] -> OnePair
  [1, 1, 1, 1, 1] -> HighCard
  _ -> error $ "no hand type for: " <> show grouping


mkHandType2 :: [Char] -> HandType
mkHandType2 cards = evalEqualGroups jokersApplied
  where
  (jokers, nonJokers) = partition (== 'J') cards
  grouping = reverse $ sort $ map length $ group $ sort nonJokers
  jokersApplied :: [Int]
  jokersApplied = case grouping of
    [] -> [length jokers]
    (highest:lower) -> (highest + length jokers) : lower

devaluateJokers :: [Int] -> [Int]
devaluateJokers = map (\n -> if n == 11 then 1 else n)

mkHand2 :: Hand -> Hand
mkHand2 hand = Hand
  { cards = cards hand
  , bid = bid hand
  , handType = mkHandType2 $ cards hand
  , cardNums = devaluateJokers $ cardNums hand
  }
