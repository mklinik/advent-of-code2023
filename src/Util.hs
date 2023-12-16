module Util where

import Text.Megaparsec
import Data.Void
import Data.Char
import Data.Functor
import System.CPUTime
import System.IO.Unsafe

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

kebabIdentifier :: Parser String
kebabIdentifier = takeWhile1P (Just "identifier") $ flip elem $ ['a'..'z']<>['-']

time :: (Show a) => a -> a
time x = unsafePerformIO $ do
  start <- getCPUTime
  print x
  end <- getCPUTime
  let elapsedSec = fromIntegral (end - start) / (10^12)
  putStrLn $ "elapsed: " <> show elapsedSec <> " s"
  return x
