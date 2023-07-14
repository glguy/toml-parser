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
    Outcome(..),

    -- * Input processing
    locatedUncons,

    -- * Actions
    token,
    token_,

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
type Action = Located String -> Context -> Outcome

data Outcome
  = Resume Context
  | LexerError (Located String)
  | EmitToken (Located Token)

-- | Representation of the current lexer state.
data Context
  = TopContext -- ^ top-level where @[[@ and @]]@ have special meaning
  | TableContext -- ^ inline table - lex key names
  | ValueContext -- ^ value lexer - lex number literals
  | MlStrContext Position [String] -- ^ position of opening delimiter and list of fragments
  | StrContext   Position [String] -- ^ position of opening delimiter and list of fragments
  deriving Show

-- | Add a literal fragment of a string to the current string state.
strFrag :: Action
strFrag (Located _ s) = \case
  StrContext   p acc -> Resume (StrContext   p (s : acc))
  MlStrContext p acc -> Resume (MlStrContext p (s : acc))
  _                  -> error "strFrag: panic"

-- | End the current string state and emit the string literal token.
endStr :: Action
endStr (Located _ x) = \case
    StrContext   p acc -> EmitToken (Located p (TokString   (concat (reverse (x : acc)))))
    MlStrContext p acc -> EmitToken (Located p (TokMlString (concat (reverse (x : acc)))))
    _                  -> error "endStr: panic"

-- | Start a basic string literal
startStr :: Action
startStr (Located p _) _ = Resume (StrContext p [])

-- | Start a multi-line basic string literal
startMlStr :: Action
startMlStr (Located p _) _ = Resume (MlStrContext p [])

-- | Resolve a unicode escape sequence and add it to the current string literal
unicodeEscape :: Action
unicodeEscape (Located p lexeme) ctx =
  case readHex (drop 2 lexeme) of
    [(n,_)] | 0xd800 <= n, n < 0xe000 -> LexerError (Located p "non-scalar unicode escape")
      | n >= 0x110000                 -> LexerError (Located p "unicode escape too large")
      | otherwise                     -> strFrag (Located p [chr n]) ctx
    _                                 -> error "unicodeEscape: panic"

-- | Emit a token ignoring the current lexeme
token_ :: Token -> Action
token_ t x _ = EmitToken (t <$ x)

-- | Emit a token using the current lexeme
token :: (String -> Token) -> Action
token f x _ = EmitToken (f <$> x)

-- | Attempt to parse the current lexeme as a date-time token.
timeValue ::
  ParseTime a =>
  String       {- ^ description for error messages -} ->
  [String]     {- ^ possible valid patterns        -} ->
  (a -> Token) {- ^ token constructor              -} ->
  Action
timeValue description patterns constructor (Located p str) _ =
  case asum [parseTimeM False defaultTimeLocale pat str | pat <- patterns] of
    Nothing -> LexerError (Located p ("malformed " ++ description))
    Just t  -> EmitToken (Located p (constructor t))

-- | Pop the first character off a located string if it's not empty.
-- The resulting 'Int' will either be the ASCII value of the character
-- or @1@ for non-ASCII Unicode values. To avoid a clash, @\x1@ is
-- remapped to @0@.
locatedUncons :: Located String -> Maybe (Int, Located String)
locatedUncons Located { locPosition = p, locThing = str } =
  case str of
    "" -> Nothing
    x:xs
      | rest `seq` False -> undefined
      | x == '\1' -> Just (0,     rest)
      | isAscii x -> Just (ord x, rest)
      | otherwise -> Just (1,     rest)
      where
        rest = Located { locPosition = move x p, locThing = xs }

-- | Generate the correct terminating token given the current lexer state.
eofToken :: Context -> Located String -> Either (Located String) (Located Token, Located String)
eofToken (MlStrContext p _) _ = Left (Located p "unterminated multi-line string literal")
eofToken (StrContext   p _) _ = Left (Located p "unterminated string literal")
eofToken _                  t = Right (TokEOF <$ t, t)
