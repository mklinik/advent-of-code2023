{-# LANGUAGE RecordWildCards #-}
module Day19 where

import Text.Megaparsec
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Util

day19 :: String -> Int
day19 input = solve $ myParse puzzleP input

day19_2 :: String -> Integer
day19_2 input = symSolve $ myParse symPuzzleP input

data Result = Accept | Reject | SendTo String
  deriving (Show, Eq, Ord)

data Part = Part
  { x :: Int
  , m :: Int
  , a :: Int
  , s :: Int
  }
  deriving (Show, Eq)

data SymPart = SymPart
  { spx :: (Int, Int)
  , spm :: (Int, Int)
  , spa :: (Int, Int)
  , sps :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

defaultSymPart :: SymPart
defaultSymPart = SymPart
  { spx = (1, 4000)
  , spm = (1, 4000)
  , spa = (1, 4000)
  , sps = (1, 4000)
  }

type WorkflowF = Part -> Result

data Workflow = Workflow
  { name :: String
  , f :: WorkflowF
  }

data SymWorkflow = SymWorkflow
  { swName :: String
  , swRules :: [SymRule]
  }

data Rule
  = EvalRule (Part -> Int, Int -> Bool, Result)
  | DefaultRule Result

data SymRule
  = SymEvalRule (Char, Char, Int, Result)
  | SymDefaultRule Result

resultP :: Parser Result
resultP =
      Accept <$ single 'A'
  <|> Reject <$ single 'R'
  <|> SendTo <$> identifier

comparatorP =
      (<) <$ single '<'
  <|> (>) <$ single '>'

ruleP :: Parser Rule
ruleP =
      try evalRuleP
  <|> defaultRuleP
  where
  defaultRuleP = DefaultRule <$> resultP
  evalRuleP = do
    accessor <- accessorP
    compare <- comparatorP
    num <- numP
    single ':'
    result <- resultP
    return $ EvalRule
      ( accessor
      , \x -> x `compare` num
      , result
      )

symRuleP :: Parser SymRule
symRuleP =
      try symEvalRuleP
  <|> symDefaultRuleP
  where
  symDefaultRuleP = SymDefaultRule <$> resultP
  symEvalRuleP = do
    accessor <- satisfy (`elem` "xmas")
    compare <- satisfy (`elem` "<>")
    threshold <- numP
    single ':'
    result <- resultP
    return $ SymEvalRule
      ( accessor
      , compare
      , threshold
      , result
      )

accessorP :: Parser (Part -> Int)
accessorP =
      x <$ single 'x'
  <|> m <$ single 'm'
  <|> a <$ single 'a'
  <|> s <$ single 's'

workflowP :: Parser Workflow
workflowP = do
  name <- identifier
  single '{'
  rules <- ruleP `sepBy` single ','
  single '}'
  return Workflow
    { name = name
    , f = mkWorkflowF rules
    }

symWorkflowP :: Parser SymWorkflow
symWorkflowP = do
  name <- identifier
  single '{'
  rules <- symRuleP `sepBy` single ','
  single '}'
  return SymWorkflow
    { swName = name
    , swRules = rules
    }

mkWorkflowF :: [Rule] -> WorkflowF
mkWorkflowF [] _ = error "reached end of workflow spec"
mkWorkflowF (DefaultRule r:_) _ = r
mkWorkflowF (EvalRule (accessor,predicate,result):rest) part =
  if predicate (accessor part)
  then result
  else mkWorkflowF rest part

partP :: Parser Part
partP = do
  chunk "{x="
  x <- numP
  chunk ",m="
  m <- numP
  chunk ",a="
  a <- numP
  chunk ",s="
  s <- numP
  single '}'
  return Part
    { x = x
    , m = m
    , a = a
    , s = s
    }

data Puzzle = Puzzle
  { workflows :: [Workflow]
  , parts :: [Part]
  }

puzzleP :: Parser Puzzle
puzzleP = do
  workflows <- many (workflowP <* ws)
  parts <- many (partP <* ws)
  eof
  return Puzzle
    { workflows = workflows
    , parts = parts
    }

data SymPuzzle = SymPuzzle
  { symWorkflows :: [SymWorkflow]
  }

symPuzzleP :: Parser SymPuzzle
symPuzzleP = do
  workflows <- many (symWorkflowP <* ws)
  _ <- many (partP <* ws)
  eof
  return SymPuzzle
    { symWorkflows = workflows
    }

sumPart :: Part -> Int
sumPart Part{..} = x + m + a + s

process :: Puzzle -> Part -> Maybe Part
process puzzle part = go "in"
  where
  ws = Map.fromList [ (name flow, f flow) | flow <- workflows puzzle ]
  go curName = case Map.lookup curName ws of
    Just fun -> case fun part of
      SendTo newName -> go newName
      Accept -> Just part
      Reject -> Nothing
    Nothing -> error ("unknown workflow " <> curName)

solve :: Puzzle -> Int
solve puzzle = sum $ map sumPart $ catMaybes $ map (process puzzle) (parts puzzle)

symEvalWorkflow :: [SymRule] -> SymPart -> [(SymPart, Result)]
symEvalWorkflow [] _ = error "reached end of workflow"
symEvalWorkflow (SymDefaultRule r : _) part = [(part, r)]
symEvalWorkflow (SymEvalRule (field, compare, threshold, r) : rest) part =
  [(thenPart, r)] <> symEvalWorkflow rest elsePart
  where
  (thenPart, elsePart) = symEvalCondition field compare threshold part

symEvalCondition :: Char -> Char -> Int -> SymPart -> (SymPart, SymPart)
symEvalCondition field compare threshold part = (thenPart, elsePart)
  where
  oldVal = case field of
    'x' -> spx part
    'm' -> spm part
    'a' -> spa part
    's' -> sps part
    _ -> error "symEvalCondition: unknown field"
  (thenVal, elseVal) = updateInterval oldVal compare threshold
  thenPart = case field of
    'x' -> part { spx = thenVal }
    'm' -> part { spm = thenVal }
    'a' -> part { spa = thenVal }
    's' -> part { sps = thenVal }
    _ -> error "symEvalCondition: unknown field"
  elsePart = case field of
    'x' -> part { spx = elseVal }
    'm' -> part { spm = elseVal }
    'a' -> part { spa = elseVal }
    's' -> part { sps = elseVal }
    _ -> error "symEvalCondition: unknown field"

updateInterval :: (Int, Int) -> Char -> Int -> ((Int, Int), (Int, Int))
updateInterval (lower, upper) '>' threshold =
  ( (max (threshold + 1) lower, upper)
  , (lower, min threshold upper)
  )
updateInterval (lower, upper) '<' threshold =
  ( (lower, min (threshold - 1) upper)
  , (max threshold lower, upper)
  )

symProcess :: SymPuzzle -> [SymPart]
symProcess puzzle = go "in" defaultSymPart
  where
  ws = Map.fromList [ (swName flow, swRules flow) | flow <- symWorkflows puzzle ]
  go curName part = case Map.lookup curName ws of
    Just rules ->
      let result = symEvalWorkflow rules part
      in [part' | (part', Accept) <- result] <> concat [go nextName part' | (part', SendTo nextName) <- result]
    Nothing -> error "unknown workflow"

symSolve :: SymPuzzle -> Integer
symSolve puzzle = sum $ map combinations $ symProcess puzzle
  where
  combinations part = product $ map range [spx part, spm part, spa part, sps part]
  range :: (Int, Int) -> Integer
  range (lower, upper) = fromIntegral $ max 0 (1 + upper - lower)
