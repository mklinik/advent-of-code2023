module Day12 where

import Text.Megaparsec
import Data.List
import Debug.Trace
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import Util

day12 :: String -> Int
day12 input = time $ sum $ map evalRow $ myParse day12P input

day12_2 :: String -> Int
day12_2 input = sum $ map evalRow $ map unfoldRow $ myParse day12P input

unfoldRow :: Row -> Row
unfoldRow (record, report) = (intercalate "?" (replicate 5 record), concat $ replicate 5 report)

type ConditionRecord = String
type DamageReport = [Int]

conditionRecordP :: Parser String
conditionRecordP = takeWhileP Nothing (`elem` ".#?")

damageReportP :: Parser DamageReport
damageReportP = numP `sepBy` single ','

type Row = (ConditionRecord, DamageReport)

rowP :: Parser Row
rowP = (,) <$> conditionRecordP <* ws <*> damageReportP

day12P :: Parser [Row]
day12P = many (rowP <* single '\n')

type St = [MyState]
type NFA = St -> Char -> [St]

evalRow :: Row -> Int
evalRow (record, report) = MultiSet.occur [Hash 1] $ process (mkNFA report) record

delta :: NFA -> MultiSet St -> Char -> MultiSet St
delta automaton states input = MultiSet.concatMap (\s -> automaton s input) states

process :: St -> String -> MultiSet St
process initialState input = foldl (delta step) (MultiSet.singleton initialState) input

data MyState = Dot | Hash Int
  deriving (Show, Eq, Ord)

mkNFA :: DamageReport -> St
mkNFA report = ([Dot] <> intersperse Dot hashStates)
  where
  hashStates :: [MyState]
  hashStates = map Hash report

step :: NFA
step [] _ = error "should never happen: exhausted states"
step s@(Dot:rest) input =
  case input of
    '.' -> [s]
    '#' -> [rest]
    '?' -> [s, rest]
step s@(Hash 1:[]) input =  -- final state
  case input of
    '#' -> []
    '.' -> [s]
    '?' -> [s]
step s@(Hash n:rest) input
  | n > 1 =
    case input of
      '.' -> []
      '#' -> [Hash (n-1):rest]
      '?' -> [Hash (n-1):rest]
  | n == 1 =
    case input of
      '.' -> [rest]
      '#' -> []
      '?' -> [rest]
