module Day2 where

import Text.Megaparsec
import Data.Functor
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Map.Merge.Lazy
import Data.Functor.Identity

import Util

day2 :: String -> Int
day2 input = sum $ map fst $ filter snd $ map gamePossible $ myParse day2P input

day2_2 :: String -> Int
day2_2 input = sum $ map gamePower $ myParse day2P input

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

colorP :: Parser Color
colorP =
      Red <$ chunk "red"
  <|> Green <$ chunk "green"
  <|> Blue <$ chunk "blue"

type Draw = [(Int, Color)]

drawP :: Parser Draw
drawP = ((,) <$> numP <* ws <*> colorP) `sepBy` (single ',' <* ws)

type Game = (Int, [Draw])

gameP :: Parser Game
gameP = do
  void $ chunk "Game "
  gameNo <- numP
  void $ single ':' <* ws
  draws <- drawP `sepBy` (single ';' <* ws)
  return (gameNo, draws)

day2P :: Parser [Game]
day2P = many (gameP <* single '\n') <* eof

availableCubes :: Map Color Int
availableCubes = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

drawPossible :: Draw -> Bool
drawPossible cubes = and
  [ num <= maxOfThisColor
  | (num, color) <- cubes
  , let maxOfThisColor = fromJust (Map.lookup color availableCubes)
  ]

gamePossible :: Game -> (Int, Bool)
gamePossible game = fmap (and . map drawPossible) game

minimumPossible :: Draw -> Map Color Int
minimumPossible draw = Map.fromList [(c,n)|(n,c) <- draw]

minimumPossibleGame :: Game -> (Int, Map Color Int)
minimumPossibleGame (gameNo, draws) = (gameNo, foldr mergeWithMax Map.empty $ map minimumPossible draws)

mergeWithMax l r = merge preserveMissing preserveMissing (zipWithAMatched $ \_k x y -> Identity $ max x y) l r

power :: Map Color Int -> Int
power m = product [n | (_, n) <- Map.toList m]

gamePower :: Game -> Int
gamePower = power . snd . minimumPossibleGame
