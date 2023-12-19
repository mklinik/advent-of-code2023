{-# LANGUAGE RecordWildCards #-}
module Day16 where

import Text.Megaparsec
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Debug.Trace

import Util

day16 :: String -> Int
day16 input = execute (myParse boardP input) (Beam (1,1) R)

day16_2 :: String -> Int
day16_2 input = maximum $ map run $ zip [1..] $ possibleStarts board
  where
  board = myParse boardP input
  run :: (Int, Beam) -> Int
  run (n, startBeam) = time $ execute board (trace (show n) startBeam)

type Board = Map (Int, Int) Char

fieldP :: Parser ((Int, Int), Char)
fieldP = (,) <$> (swap <$> myPosP) <*> satisfy (`elem` ".|-/\\")

boardP :: Parser Board
boardP = Map.fromList <$> many (fieldP <* optional ws) <* eof

data Direction = U | D | L | R
  deriving (Eq, Show, Ord)

data Beam = Beam
  { pos :: (Int, Int)
  , dir :: Direction
  }
  deriving (Eq, Show, Ord)

ppBeam :: Beam -> String
ppBeam Beam{..} = show pos <> " " <> show dir

type Configuration = Set Beam

-- | Take one step in the direction the beam is currently facing
shine :: Board -> Beam -> Maybe Beam
shine board b@Beam{pos=(x,y),..} = newBeam
  where
  newPos = case dir of
    U -> (x, y-1)
    D -> (x, y+1)
    L -> (x-1, y)
    R -> (x+1, y)
  newBeam = case Map.lookup newPos board of
    Nothing -> Nothing
    Just _ -> Just b { pos = newPos }

-- | Change direction depending on the mirror
reflect :: Char -> Beam -> Beam
reflect tile b@Beam{..} = b { dir = newDirection }
  where
  newDirection = case (tile, dir) of
    ('/', U) -> R
    ('/', D) -> L
    ('/', L) -> D
    ('/', R) -> U

    ('\\', U) -> L
    ('\\', D) -> R
    ('\\', L) -> U
    ('\\', R) -> D

    _ -> error "reflect: unknown tile"

-- | Split the beam in two, depending on the splitter. Does not move the beam
split :: Char -> Beam -> [Beam]
split tile b@Beam{..} = case (tile, dir) of
  ('|', U) -> [b]
  ('|', D) -> [b]
  ('|', L) -> [b {dir = U}, b {dir = D}]
  ('|', R) -> [b {dir = U}, b {dir = D}]

  ('-', U) -> [b {dir = L}, b {dir = R}]
  ('-', D) -> [b {dir = L}, b {dir = R}]
  ('-', L) -> [b]
  ('-', R) -> [b]

  _ -> error "split: unknown tile"

-- | Shine one step in the current direction, and then act according to the encountered tile
stepBeam :: Board -> Beam -> Set Beam
stepBeam board beam = Set.fromList $ catMaybes $ map (shine board) $
  case Map.lookup (pos beam) board of
    Nothing -> []
    Just tile -> case tile of
      '.' -> [beam]
      '/' -> [reflect tile beam]
      '\\' -> [reflect tile beam]
      '|' -> split tile beam
      '-' -> split tile beam
      _ -> error "stepBeam: unknown tile"

step :: Board -> Configuration -> Configuration
step board beams = Set.unions $ Set.map (stepBeam board) beams

execute :: Board -> Beam -> Int
execute board startBeam = Set.size $ Set.map pos $ go Set.empty (Set.singleton startBeam)
  where
  go acc beams =
    let
      newBeams = step board beams
      newAcc = acc `Set.union` beams
    in
      if newAcc == acc
      then acc
      else go newAcc (newBeams Set.\\ acc)

boardDimensions :: Board -> (Int, Int)
boardDimensions board = maximum $ map fst $ Map.toList board

possibleStarts :: Board -> [Beam]
possibleStarts board = fromLeft <> fromRight <> fromTop <> fromBot
  where
  (maxX, maxY) = boardDimensions board
  fromLeft = [Beam (1, y) R | y <- [1..maxY]]
  fromRight = [Beam (maxX, y) L | y <- [1..maxY]]
  fromTop = [Beam (x, 1) D | x <- [1..maxX]]
  fromBot = [Beam (x, maxY) U | x <- [1..maxX]]
