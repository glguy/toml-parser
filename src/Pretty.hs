module Pretty (prettyKey) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List (intercalate)
import Text.Printf (printf)

prettyKey :: [String] -> String
prettyKey = intercalate "." . map prettySimpleKey

prettySimpleKey :: String -> String
prettySimpleKey str
  | all isBareKey str = str
  | otherwise         = '"' : quoteString str

isBareKey :: Char -> Bool
isBareKey x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '-' || x == '_'

quoteString :: String -> String
quoteString = \case
  ""        -> "\"" -- terminator
  '"'  : xs -> '\\' : '"'  : quoteString xs
  '\\' : xs -> '\\' : '\\' : quoteString xs
  '\b' : xs -> '\\' : 'b'  : quoteString xs
  '\f' : xs -> '\\' : 'f'  : quoteString xs
  '\n' : xs -> '\\' : 'n'  : quoteString xs
  '\r' : xs -> '\\' : 'r'  : quoteString xs
  '\t' : xs -> '\\' : 't'  : quoteString xs
  x    : xs
    | isPrint x     -> x : quoteString xs
    | x <= '\xffff' -> printf "\\u%04X%s" (ord x) (quoteString xs)
    | otherwise     -> printf "\\U%08X%s" (ord x) (quoteString xs)
