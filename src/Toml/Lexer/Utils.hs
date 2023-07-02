{-|
Module      : Toml.Lexer.Utils
Description : Wrapper and actions for generated lexer
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a custom engine for the Alex generated
lexer. This lexer drive provides nested states, unicode support,
and file location tracking.

The various states of this module are needed to deal with the varying
lexing rules while lexing values, keys, and string-literals.

-}
module Toml.Lexer.Utils (

    -- * Types
    Action,
    Context(..),

    -- * Input processing
    locatedUncons,

    -- * Actions
    value,
    value_,
    token,
    token_,

    squareO,
    squareC,
    curlyO,
    curlyC,

    equals,
    timeValue,
    eofToken,

    -- * String literals
    strFrag,
    startMlStr,
    startStr,
    endStr,
    unicodeEscape,


    ) where

import Control.Monad.Trans.State.Strict (State, state)
import Data.Char (ord, chr, isAscii)
import Data.Foldable (asum)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Numeric (readHex)

import Toml.Located (Located(..))
import Toml.Position (move, Position)
import Toml.Lexer.Token (Token(..))

-- | Type of actions associated with lexer patterns
type Action = Located String -> State [Context] [Located Token]

-- | Representation of the current lexer state.
data Context
  = ListContext Position -- ^ processing an inline list, lex values
  | TableContext Position -- ^ processing an inline table, don't lex values
  | ValueContext -- ^ processing after an equals, lex one value
  | MlStrContext Position [String] -- ^ position of opening delimiter and list of fragments
  | StrContext   Position [String] -- ^ position of opening delimiter and list of fragments
  deriving Show

-- | Add a literal fragment of a string to the current string state.
strFrag :: Action
strFrag s = state \case
  StrContext   p acc : st -> ([], StrContext   p (locThing s : acc) : st)
  MlStrContext p acc : st -> ([], MlStrContext p (locThing s : acc) : st)
  _                       -> error "strFrag: panic"

-- | End the current string state and emit the string literal token.
endStr :: Action
endStr x = state \case
    StrContext   p acc : st -> ([Located p (TokString   (concat (reverse (locThing x : acc))))], st)
    MlStrContext p acc : st -> ([Located p (TokMlString (concat (reverse (locThing x : acc))))], st)
    _                       -> error "endStr: panic"

-- | Start a basic string literal
startStr :: Action
startStr t = state \case
  ValueContext : st -> ([], StrContext (locPosition t) [] : st)
  st                -> ([], StrContext (locPosition t) [] : st)

-- | Start a multi-line basic string literal
startMlStr :: Action
startMlStr t = state \case
  ValueContext : st -> ([], MlStrContext (locPosition t) [] : st)
  st                -> ([], MlStrContext (locPosition t) [] : st)

-- | Resolve a unicode escape sequence and add it to the current string literal
unicodeEscape :: Action
unicodeEscape (Located p lexeme) =
  case readHex (drop 2 lexeme) of
    [(n,_)] | 0xd800 <= n, n < 0xe000 -> pure [Located p (TokError "non-scalar unicode escape")]
      | n >= 0x110000                 -> pure [Located p (TokError "unicode escape too large")]
      | otherwise                     -> strFrag (Located p [chr n])
    _                                 -> error "unicodeEscape: panic"

-- | Record an @=@ token and update the state
equals :: Action
equals t = state \case
  st -> ([TokEquals <$ t], ValueContext : st)

-- | Record an opening square bracket and update the state
squareO :: Action
squareO t = state \case
  ValueContext  : st -> ([TokSquareO <$ t], ListContext (locPosition t) : st)
  ListContext p : st -> ([TokSquareO <$ t], ListContext (locPosition t): ListContext p : st)
  st                 -> ([TokSquareO <$ t], st)

-- | Record a closing square bracket and update the state
squareC :: Action
squareC t = state \case
  ListContext _ : st -> ([TokSquareC <$ t], st)
  st                 -> ([TokSquareC <$ t], st)

-- | Record an opening curly bracket and update the state
curlyO :: Action
curlyO t = state \case
  ValueContext  : st -> ([TokCurlyO <$ t], TableContext (locPosition t) : st)
  ListContext p : st -> ([TokCurlyO <$ t], TableContext (locPosition t) : ListContext p : st)
  st                 -> ([TokCurlyO <$ t], st)

-- | Record a closing curly bracket and update the state
curlyC :: Action
curlyC t = state \case
  TableContext _ : st -> ([TokCurlyC <$ t], st)
  st                  -> ([TokCurlyC <$ t], st)

-- | Emit a token ignoring the current lexeme
token_ :: Token -> Action
token_ t x = pure [t <$ x]

-- | Emit a token using the current lexeme
token :: (String -> Token) -> Action
token f x = pure [f <$> x]

-- | Emit a value token and update the current state
value_ :: Token -> Action
value_ t = value (const t)

-- | Emit a value token using the current lexeme and update the current state
value :: (String -> Token) -> Action
value f x = state \st ->
  case st of
    ValueContext : st' -> ([f <$> x], st')
    _                  -> ([f <$> x], st )

-- | Attempt to parse the current lexeme as a date-time token.
timeValue ::
  ParseTime a =>
  String       {- ^ description for error messages -} ->
  [String]     {- ^ possible valid patterns        -} ->
  (a -> Token) {- ^ token constructor              -} ->
  Action
timeValue description patterns constructor = value \str ->
  case asum [parseTimeM False defaultTimeLocale pattern str | pattern <- patterns] of
    Nothing -> TokError ("malformed " ++ description)
    Just t  -> constructor t

-- | Pop the first character off a located string if it's not empty.
-- The resulting 'Int' will either be the ASCII value of the character
-- or @1@ for non-ASCII Unicode values. To avoid a clash, @\x1@ is
-- remapped to @0@.
locatedUncons :: Located String -> Maybe (Int, Located String)
locatedUncons Located { locPosition = p, locThing = str } =
  case str of
    "" -> Nothing
    x:xs
      | x == '\1' -> Just (0,     rest)
      | isAscii x -> Just (ord x, rest)
      | otherwise -> Just (1,     rest)
      where
        rest = Located { locPosition = move x p, locThing = xs }

-- | Generate the correct terminating token given the current lexer state.
eofToken :: [Context] -> Located String -> Located Token
eofToken (MlStrContext p _ : _) _ = Located p (TokError "unterminated multi-line string literal")
eofToken (StrContext   p _ : _) _ = Located p (TokError "unterminated string literal")
eofToken (ListContext  p   : _) _ = Located p (TokError "unterminated '['")
eofToken (TableContext p   : _) _ = Located p (TokError "unterminated '{'")
eofToken (ValueContext     : s) t = eofToken s t
eofToken _                      t = TokEOF <$ t
