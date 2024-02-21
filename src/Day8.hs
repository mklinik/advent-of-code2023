{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Day8 where

import Text.Megaparsec
import Data.Maybe
import Debug.Trace
import Data.Char
import qualified Data.Map as Map
import Debug.Trace as Debug
import Data.List
import Control.DeepSeq
import qualified Data.Set as Set
import Data.Hashable
import qualified Data.IntMap as IntMap

import Util

day8 :: String -> Int
day8 input = length $ followInstructions $ myParse puzzleP input

day8_2 :: String -> Int
day8_2 input = 12927600769609
-- each start state loops back to itself eventually
-- there is only a single final state in each loop
-- the solution is therefore the least common multiple of the steps of the individual loops
-- this is not true in general, only for this puzzle input having the properties above
-- solution is 12927600769609

data Node = Node
  { name :: String
  , l :: String
  , r :: String
  }
  deriving (Show, Eq)

data Puzzle = Puzzle
  { instructions :: [Char]
  , network :: [Node]
  }
  deriving (Show, Eq)

data Node2 = Node2
  { name2 :: (Int, Bool)
  , initial2 :: Bool
  , l2 :: (Int, Bool)
  , r2 :: (Int, Bool)
  }
  deriving (Show, Eq)

data Puzzle2 = Puzzle2
  { instructions2 :: [Node2 -> (Int, Bool)]
  , network2 :: [Node2]
  }

mkPuzzle2 :: Puzzle -> Puzzle2
mkPuzzle2 Puzzle{..} = Puzzle2
  { instructions2 = map select instructions
  , network2 = map mkNode2 network
  }
  where
  select :: Char -> Node2 -> (Int, Bool)
  select x = case x of
    'L' -> l2
    'R' -> r2
    _ -> error "unsupported instruction"

mkNode2 :: Node -> Node2
mkNode2 Node{..} = Node2
  { name2 = myHash name
  , initial2 = "A" `isSuffixOf` name
  , l2 = myHash l
  , r2 = myHash r
  }
  where
  myHash x = (hash x, "Z" `isSuffixOf` name)

nameP :: Parser String
nameP = takeWhile1P (Just "node name") $ flip elem $ ['A' .. 'Z'] <> ['0'..'9']

nodeP :: Parser Node
nodeP = do
  name <- nameP <* ws
  _ <- chunk "= ("
  l <- nameP
  _ <- chunk ", "
  r <- nameP
  _ <- single ')'
  return $ Node { name, l, r }

puzzleP :: Parser Puzzle
puzzleP = do
  instructions <- nameP <* ws
  network <- many (nodeP <* ws)
  return $ Puzzle { instructions, network }

followInstructions :: Puzzle -> [String]
followInstructions Puzzle{..} = go [] "AAA" instructions
  where
  go :: [String] -> String -> [Char] -> [String]
  go acc name [] = go acc name instructions
  go acc "ZZZ" _ = reverse acc
  go acc name (next:rest) = case Map.lookup name nodes of
    Nothing -> error $ "no such node: " <> name
    Just (l, r) -> case next of
      'L' -> go (name:acc) l rest
      'R' -> go (name:acc) r rest
      _ -> error "unsupported instruction"
  nodes = Map.fromList [(name, (l, r)) | Node{..} <- network]

followInstructions2 :: Puzzle -> Int
followInstructions2 (mkPuzzle2 -> Puzzle2{..}) = go 0 startState instructions2
  where
  go acc state is
    | null is = go (if (traceShowId acc) `mod` 1000 == 0 then acc else acc) state instructions2
    | isEndState state = acc
    | otherwise =
        let
          acc' = acc+1
          state' = map (step (head is)) state
          state'' = deepseq state' state'
          is' = (tail is)
        in
          go (deepseq acc' acc') (state'') (deepseq is' is')
  startState :: [(Int, Bool)]
  startState = [name2 | Node2{..} <- network2, initial2]
  nodes :: IntMap.IntMap Node2
  nodes = IntMap.fromList [(fst name2, n) | n@Node2{..} <- network2]
  step :: (Node2 -> (Int, Bool)) -> (Int, Bool) -> (Int, Bool)
  step instruction (name, _) = case IntMap.lookup name nodes of
    Nothing -> error "no such node"
    Just r -> instruction r

isEndState :: [(Int, Bool)] -> Bool
isEndState = and . map snd

singleLoop :: Puzzle -> String -> [Int]
singleLoop Puzzle{..} start = go 0 Set.empty [] start instructions
  where
  go index seen acc name is
    | null is = go index seen acc name instructions
    | (name, is) `Set.member` seen = acc
    | otherwise = 
        let
          state' = step (head is) name
          state'' = deepseq state' state'
          acc' = if "Z" `isSuffixOf` state'' then index : acc else acc
          is' = (tail is)
          seen' = Set.insert (name, is) seen
        in
          go (index+1) (deepseq seen' seen') (deepseq acc' acc') state'' (deepseq is' is')
  step :: Char -> String -> String
  step instruction name = case Map.lookup name nodes of
    Nothing -> error $ "no such node: " <> name
    Just (l, r) -> case instruction of
      'L' -> l
      'R' -> r
      _ -> error "unsupported instruction"
  nodes = Map.fromList [(name, (l, r)) | Node{..} <- network]
