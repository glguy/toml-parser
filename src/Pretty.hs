module Pretty (prettyKey) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List (intercalate)
import Text.Printf (printf)

prettyKey :: [String] -> String
prettyKey = intercalate "." . map prettySimpleKey

prettySimpleKey :: String -> String
prettySimpleKey str
  | all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '-' || x == '_') str =
    str
  | otherwise = '"' : quoteString str

quoteString :: String -> String
quoteString "" = "\""
quoteString (x:xs)
  | x == '"' = '\\' : '"' : quoteString xs
  | isPrint x = x : quoteString xs
  | x <= '\xffff' = '\\' : 'u' : printf "%04X" (ord x) ++ quoteString xs
  | otherwise = '\\' : 'U' : printf "%08X" (ord x) ++ quoteString xs
