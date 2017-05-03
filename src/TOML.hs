{-|
Module      : TOML
Description : Parser for the TOML configuration language
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module TOML
  ( Located(..)
  , Token(..)
  , TOMLError(..)
  , LexerError(..)
  , Value(..)
  , parseTOML
  ) where

import Components
import Lexer
import Parser
import Tokens
import Value

import Control.Monad
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception

data TOMLError
  = ParseError (Located Token)
  | OverlappingKey [Text]
  deriving (Read, Show)

instance Exception TOMLError where
  displayException (ParseError (Located pos token)) =
    show (posLine pos) ++ ":" ++ show (posColumn pos) ++
    ": unexpected " ++ showToken token
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
    DayTok dt       -> "local date " ++ show dt
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
    Unterminated t -> "unterminated " ++ showToken t
    BadEscape    -> "bad escape sequence"
    NoMatch c    -> "unexpected ‘" ++ [c] ++ "’"

parseTOML :: Text -> Either TOMLError [(Text,Value)]
parseTOML = mapLeft OverlappingKey . componentsToTable
        <=< mapLeft ParseError     . parseComponents . scanTokens
  where
    mapLeft f (Left e)  = Left (f e)
    mapLeft _ (Right x) = Right x
