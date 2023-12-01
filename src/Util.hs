module Util where

import Text.Megaparsec
import Data.Void

type Parser a = Parsec Void String a

myParse :: Parser a -> String -> a
myParse parser input = case parse parser "input" input of
  Right result -> result
  Left err -> error $ errorBundlePretty err
