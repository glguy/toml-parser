{-|
Module      : TOML.Tokens
Description : Internal: Token type and operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the token type used in the lexer and
parser and provides the extra pass to insert layout tokens.
-}
module TOML.Tokens
  ( Token(..)
  , LexerError(..)
  ) where

import Data.Text (Text)
import Data.Time


-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = StringToken  Text        -- ^ string literal
  | BareKeyToken Text        -- ^ bare table key
  | IntegerToken Integer     -- ^ integer literal
  | DoubleToken  Double      -- ^ floating   -point literal
  | ZonedTimeToken ZonedTime -- ^ offset date-time
  | LocalTimeToken LocalTime -- ^ local date-time
  | TimeOfDayToken TimeOfDay -- ^ local time
  | DayToken     Day         -- ^ local date
  | CommaToken               -- ^ @,@
  | PeriodToken              -- ^ @.@
  | LeftBracketToken         -- ^ @[@
  | RightBracketToken        -- ^ @[@
  | LeftBraceToken           -- ^ @{@
  | RightBraceToken          -- ^ @}@
  | EqualToken               -- ^ @=@
  | TrueToken                -- ^ @true@
  | FalseToken               -- ^ @false@
  | ErrorToken LexerError    -- ^ lexical error
  | EofToken                 -- ^ end-of-file
  deriving (Read, Show)


-- | Errors possible in the course of lexing
data LexerError
  = UntermString -- ^ unterminated string literal
  | BadEscape    -- ^ invalid escape sequence
  | NoMatch Char -- ^ no matching lexer rule
  deriving (Read, Show)
