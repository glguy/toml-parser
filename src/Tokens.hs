{-|
Module      : Tokens
Description : Token type and operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the token type used in the lexer and
parser and provides the extra pass to insert layout tokens.
-}
module Tokens
  ( Token(..)
  , Located(..)
  , Position(..)
  , LexerError(..)
  ) where

import Data.Text (Text)
import Data.Time

-- | A position in a text file
data Position = Position
  { posIndex, posLine, posColumn :: {-# UNPACK #-} !Int }
  deriving (Read, Show)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: {-# UNPACK #-} !Position
  , locThing    :: !a
  }
  deriving (Read, Show)

instance Functor Located where
  fmap f (Located p x) = Located p (f x)

-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = String Text
  | BareKey Text
  | Integer Integer
  | Double Double
  | ZonedTimeTok ZonedTime
  | LocalTimeTok LocalTime
  | TimeOfDayTok TimeOfDay
  | DayTok       Day
  | Comma
  | Period
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  | EqualSign
  | TrueToken
  | FalseToken
  | Error LexerError
  | EOF

  deriving (Read, Show)

-- | Types of lexical errors
data LexerError
  = UntermString
  | BadEscape
  | NoMatch Char
  deriving (Read, Show)
