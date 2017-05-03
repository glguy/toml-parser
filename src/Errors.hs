{-|
Module      : Errors
Description : Errors that can occur while processing TOML
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

data TOMLError
  = Unexpected   (Located Token) -- ^ unexpected token while parser
  | Unterminated (Located Token) -- ^ unterminated token while parser
  | OverlappingKey [Text]        -- ^ ambiguous table entry
  deriving (Read, Show)

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

showToken :: Token -> String
showToken t =
  case t of
    String{}     -> "string literal"
    BareKey k    -> "table key ‘" ++ Text.unpack k ++ "’"
    Integer i    -> "integer " ++ show i
    Double d     -> "float " ++ show d
    ZonedTimeTok dt -> "offset date-time " ++ show dt
    LocalTimeTok dt -> "local data-time " ++ show dt
    DayTok       dt -> "local date " ++ show dt
    TimeOfDayTok dt -> "local time " ++ show dt
    Comma        -> "‘,’"
    Period       -> "‘.’"
    LeftBracket  -> "‘[’"
    RightBracket -> "‘]’"
    LeftBrace    -> "‘{’"
    RightBrace   -> "‘}’"
    EqualSign    -> "‘=’"
    TrueToken    -> "‘true’"
    FalseToken   -> "‘false’"
    Error e      -> "lexical error: " ++ showLexerError e
    EOF          -> "end-of-file"

showLexerError :: LexerError -> String
showLexerError e =
  case e of
    UntermString -> "unterminated string literal"
    BadEscape    -> "bad escape sequence"
    NoMatch c    -> "unexpected ‘" ++ [c] ++ "’"

