module Day12 where

import Text.Megaparsec
import Data.List
import Debug.Trace

import Util

day12 :: String -> Int
day12 input = sum $ map (time . solveRow) $ myParse day12P input

data Condition = O | D | U
  deriving (Show, Eq)

type ConditionRecord = [Condition]

type DamageReport = [Int]

conditionP :: Parser Condition
conditionP =
      O <$ single '.'
  <|> D <$ single '#'
  <|> U <$ single '?'

conditionRecordP :: Parser ConditionRecord
conditionRecordP = many conditionP

damageReportP :: Parser DamageReport
damageReportP = numP `sepBy` single ','

rowP :: Parser (ConditionRecord, DamageReport)
rowP = (,) <$> conditionRecordP <* ws <*> damageReportP

day12P :: Parser [(ConditionRecord, DamageReport)]
day12P = many (rowP <* single '\n')

mkReport :: ConditionRecord -> DamageReport
mkReport cs = map length damagedGroups
  where
  damagedGroups = filter (\gr -> head gr == D) groups
  groups = group cs

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]

fillRecord :: [Condition] -> ConditionRecord -> ConditionRecord
fillRecord values conditionRecord = go [] values conditionRecord
  where
  go acc _ [] = reverse acc
  go acc (v:vs) (U:cs) = go (v:acc) vs cs
  go acc vs (c:cs) = go (c:acc) vs cs

solveRow :: (ConditionRecord, DamageReport) -> Int
solveRow (cs, givenReport) = length $ filter (\fill -> mkReport (fillRecord fill cs) == givenReport) $ possibleFills
  where
  numUnknowns = length $ filter (==U) cs
  possibleFills = comb numUnknowns [O, D]
