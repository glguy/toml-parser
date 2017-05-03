{-|
Module      : Tokens
Description : Internal: Token type and operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the token type used in the lexer and
parser and provides the extra pass to insert layout tokens.
-}
module Tokens
  ( Token(..)
  , LexerError(..)
  ) where

import Data.Text (Text)
import Data.Time
import Located

-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = String       Text      -- ^ string literal
  | BareKey      Text      -- ^ bare table key
  | Integer      Integer   -- ^ integer literal
  | Double       Double    -- ^ floating   -point literal
  | ZonedTimeTok ZonedTime -- ^ offset data-time
  | LocalTimeTok LocalTime -- ^ local date -time
  | TimeOfDayTok TimeOfDay -- ^ local time
  | DayTok       Day       -- ^ local date
  | Comma                  -- ^ @,@
  | Period                 -- ^ @.@
  | LeftBracket            -- ^ @[@
  | RightBracket           -- ^ @[@
  | LeftBrace              -- ^ @{@
  | RightBrace             -- ^ @}@
  | EqualSign              -- ^ @=@
  | TrueToken              -- ^ @true@
  | FalseToken             -- ^ @false@
  | Error LexerError       -- ^ lexical error
  | EOF                    -- ^ end        -of-file
  deriving (Read, Show)

-- | Errors possible in the course of lexing
data LexerError
  = UntermString -- ^ unterminated string literal
  | BadEscape    -- ^ invalid escape sequence
  | NoMatch Char -- ^ no matching lexer rule
  deriving (Read, Show)
