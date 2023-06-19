{-|
Module      : Toml.Pretty
Description : Human-readable representations for error messages
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides human-readable renderers for types used
in this package to assist error message production.

-}
module Toml.Pretty (prettyKey, prettySection, prettyPosition, prettyToken) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List.NonEmpty qualified as NonEmpty
import Text.Printf (printf)

import Toml.Position (Position(..))
import Toml.Raw (Key, SectionKind(..))
import Toml.Token (Token(..))

prettyKey :: Key -> String
prettyKey = concat . NonEmpty.intersperse "." . fmap prettySimpleKey

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

prettySection :: SectionKind -> Key -> String
prettySection TableKind      key = "[" ++ prettyKey key ++ "]"
prettySection ArrayTableKind key = "[[" ++ prettyKey key ++ "]]"

prettyPosition :: Position -> String
prettyPosition Position { posIndex = _, posLine = l, posColumn = c } =
    "line " ++ show l ++ " column " ++ show c

-- | Render token for human-readable error messages.
prettyToken :: Token -> String
prettyToken = \case
    TokComma            -> "','"
    TokEquals           -> "'='"
    TokPeriod           -> "'.'"
    TokSquareO          -> "'['"
    TokSquareC          -> "']'"
    Tok2SquareO         -> "'[['"
    Tok2SquareC         -> "']]]"
    TokCurlyO           -> "'{'"
    TokCurlyC           -> "'}'"
    TokComment _        -> "comment"
    TokNewline          -> "newline"
    TokBareKey key      -> "bare key: " ++ key
    TokTrue             -> "true literal"
    TokFalse            -> "false literal"
    TokString str       -> "string: " ++ show str
    TokMlString str     -> "multi-line string: " ++ show str
    TokInteger str      -> "integer: " ++ show str
    TokFloat str        -> "float: " ++ show str
    TokOffsetDateTime _ -> "offset date-time"
    TokLocalDateTime _  -> "local date-time"
    TokLocalDate _      -> "local date"
    TokLocalTime _      -> "local time"
    TokError e          -> "lexical error: " ++ e
    TokEOF              -> "end-of-input"