{-|
Module      : Errors
Description : /Internal:/ Errors that can occur while processing TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Errors where

import           Control.Exception
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text

import           Tokens
import           Located

-- | Errors that can occur while loading a TOML file.
data TOMLError
  = Unexpected   (Located Token) -- ^ unexpected token while parser
  | Unterminated (Located Token) -- ^ unterminated token while parser
  | OverlappingKey [Text]        -- ^ ambiguous table entry
  deriving (Read, Show)

-- | 'displayException' provides human-readable error message
instance Exception TOMLError where
  displayException (Unexpected (Located pos token)) =
    show (posLine pos) ++ ":" ++ show (posColumn pos) ++
    ": unexpected " ++ showToken token
  displayException (Unterminated (Located pos token)) =
    show (posLine pos) ++ ":" ++ show (posColumn pos) ++
    ": unterminated " ++ showToken token
  displayException (OverlappingKey path) =
    "multiple definitions of: " ++
    intercalate "." (map Text.unpack path)

-- | Generates a human-readable description of a token.
showToken :: Token -> String
showToken t =
  case t of
    StringToken{}     -> "string literal"
    BareKeyToken k    -> "table key ‘" ++ Text.unpack k ++ "’"
    IntegerToken i    -> "integer " ++ show i
    DoubleToken d     -> "float " ++ show d
    ZonedTimeToken dt -> "offset date-time " ++ show dt
    LocalTimeToken dt -> "local data-time " ++ show dt
    DayToken       dt -> "local date " ++ show dt
    TimeOfDayToken dt -> "local time " ++ show dt
    CommaToken        -> "‘,’"
    PeriodToken       -> "‘.’"
    LeftBracketToken  -> "‘[’"
    RightBracketToken -> "‘]’"
    LeftBraceToken    -> "‘{’"
    RightBraceToken   -> "‘}’"
    EqualToken        -> "‘=’"
    TrueToken         -> "‘true’"
    FalseToken        -> "‘false’"
    ErrorToken e      -> "lexical error: " ++ showLexerError e
    EofToken          -> "end-of-file"

-- | Generates a human-readable description of a lexical error.
showLexerError :: LexerError -> String
showLexerError e =
  case e of
    UntermString -> "unterminated string literal"
    BadEscape    -> "bad escape sequence"
    NoMatch c    -> "unexpected ‘" ++ [c] ++ "’"
