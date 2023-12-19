module Util where

import Text.Megaparsec
import Data.Void
import Data.Char
import Data.Functor
import System.CPUTime
import System.IO.Unsafe
import Text.Printf

type Parser a = Parsec Void String a

myParse :: Parser a -> String -> a
myParse parser input = case parse parser "input" input of
  Right result -> result
  Left err -> error $ errorBundlePretty err

numP :: (Num a, Read a) => Parser a
numP = read <$> takeWhile1P (Just "number") isDigit

ws :: Parser ()
ws = void $ takeWhile1P (Just "whitespace") isSpace

space :: Parser ()
space = void $ single ' '

nl :: Parser ()
nl = void $ single '\n'

type MySourcePos = (Int, Int)

mkMyPos :: SourcePos -> MySourcePos
mkMyPos p = (line, col)
  where
  line = unPos $ sourceLine p
  col = unPos $ sourceColumn p

myPosP :: Parser MySourcePos
myPosP = mkMyPos <$> getSourcePos

swap (a, b) = (b, a)

kebabIdentifier :: Parser String
kebabIdentifier = takeWhile1P (Just "identifier") $ flip elem $ ['a'..'z']<>['-']

time :: (Show a) => a -> a
time x = unsafePerformIO $ do
  start <- getCPUTime
  print x
  end <- getCPUTime
  let elapsedSec = fromIntegral (end - start) / (10^12)
  printf "elapsed: %.3f s\n" (elapsedSec :: Double)
  return x

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]
