{-|
Module      : Toml.Syntax.LexerUtils
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
module Toml.Syntax.LexerUtils (

    -- * Types
    Action,
    Context(..),
    Outcome(..),

    -- * Input processing
    locatedUncons,

    -- * Actions
    token,
    token_,
    textToken,

    timeValue,
    eofToken,

    failure,

    -- * String literals
    strFrag,
    startMlBstr,
    startBstr,
    startMlLstr,
    startLstr,
    endStr,
    unicodeEscape,
    recommendEscape,

    mkError,
    ) where

import Data.Char (ord, chr, isAscii, isControl)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Numeric (readHex)
import Text.Printf (printf)
import Toml.Syntax.Token (Token(..))
import Toml.Syntax.Position (move, Located(..), Position)

-- | Type of actions associated with lexer patterns
type Action = Located Text -> Context -> Outcome

data Outcome
  = Resume Context
  | LexerError (Located String)
  | EmitToken (Located Token)

-- | Representation of the current lexer state.
data Context
  = TopContext -- ^ top-level where @[[@ and @]]@ have special meaning
  | TableContext -- ^ inline table - lex key names
  | ValueContext -- ^ value lexer - lex number literals
  | MlBstrContext Position [Text] -- ^ multiline basic string: position of opening delimiter and list of fragments
  | BstrContext   Position [Text] -- ^ basic string: position of opening delimiter and list of fragments
  | MlLstrContext Position [Text] -- ^ multiline literal string: position of opening delimiter and list of fragments
  | LstrContext   Position [Text] -- ^ literal string: position of opening delimiter and list of fragments
  deriving Show

-- | Add a literal fragment of a string to the current string state.
strFrag :: Action
strFrag (Located _ s) = \case
  BstrContext   p acc -> Resume (BstrContext   p (s : acc))
  MlBstrContext p acc -> Resume (MlBstrContext p (s : acc))
  LstrContext   p acc -> Resume (LstrContext   p (s : acc))
  MlLstrContext p acc -> Resume (MlLstrContext p (s : acc))
  _                   -> error "strFrag: panic"

-- | End the current string state and emit the string literal token.
endStr :: Action
endStr (Located _ x) = \case
    BstrContext   p acc -> EmitToken (Located p (TokString   (Text.concat (reverse (x : acc)))))
    MlBstrContext p acc -> EmitToken (Located p (TokMlString (Text.concat (reverse (x : acc)))))
    LstrContext   p acc -> EmitToken (Located p (TokString   (Text.concat (reverse (x : acc)))))
    MlLstrContext p acc -> EmitToken (Located p (TokMlString (Text.concat (reverse (x : acc)))))
    _                  -> error "endStr: panic"

-- | Start a basic string literal
startBstr :: Action
startBstr (Located p _) _ = Resume (BstrContext p [])

-- | Start a literal string literal
startLstr :: Action
startLstr (Located p _) _ = Resume (LstrContext p [])

-- | Start a multi-line basic string literal
startMlBstr :: Action
startMlBstr (Located p _) _ = Resume (MlBstrContext p [])

-- | Start a multi-line literal string literal
startMlLstr :: Action
startMlLstr (Located p _) _ = Resume (MlLstrContext p [])

-- | Resolve a unicode escape sequence and add it to the current string literal
unicodeEscape :: Action
unicodeEscape (Located p lexeme) ctx =
  case readHex (drop 2 (Text.unpack lexeme)) of
    [(n,_)] | 0xd800 <= n, n < 0xe000 -> LexerError (Located p "non-scalar unicode escape")
      | n >= 0x110000                 -> LexerError (Located p "unicode escape too large")
      | otherwise                     -> strFrag (Located p (Text.singleton (chr n))) ctx
    _                                 -> error "unicodeEscape: panic"

recommendEscape :: Action
recommendEscape (Located p x) _ =
  LexerError (Located p (printf "control characters must be escaped, use: \\u%04X" (ord (Text.head x))))

-- | Emit a token ignoring the current lexeme
token_ :: Token -> Action
token_ t x _ = EmitToken (t <$ x)

-- | Emit a token using the current lexeme
token :: (String -> Token) -> Action
token f x _ = EmitToken (f . Text.unpack <$> x)

-- | Emit a token using the current lexeme
textToken :: (Text -> Token) -> Action
textToken f x _ = EmitToken (f <$> x)

-- | Attempt to parse the current lexeme as a date-time token.
timeValue ::
  ParseTime a =>
  String       {- ^ description for error messages -} ->
  [String]     {- ^ possible valid patterns        -} ->
  (a -> Token) {- ^ token constructor              -} ->
  Action
timeValue description patterns constructor (Located p str) _ =
  case asum [parseTimeM False defaultTimeLocale pat (Text.unpack str) | pat <- patterns] of
    Nothing -> LexerError (Located p ("malformed " ++ description))
    Just t  -> EmitToken (Located p (constructor t))

-- | Pop the first character off a located string if it's not empty.
-- The resulting 'Int' will either be the ASCII value of the character
-- or @1@ for non-ASCII Unicode values. To avoid a clash, @\x1@ is
-- remapped to @0@.
locatedUncons :: Located Text -> Maybe (Int, Located Text)
locatedUncons Located { locPosition = p, locThing = str } =
  case Text.uncons str of
    Nothing -> Nothing
    Just (x, xs)
      | rest `seq` False -> undefined
      | x == '\1' -> Just (0,     rest)
      | isAscii x -> Just (ord x, rest)
      | otherwise -> Just (1,     rest)
      where
        rest = Located { locPosition = move x p, locThing = xs }

-- | Generate the correct terminating token given the current lexer state.
eofToken :: Context -> Located Text -> Either (Located String) (Located Token, Located Text)
eofToken (MlBstrContext p _) _ = Left (Located p "unterminated multi-line basic string")
eofToken (BstrContext   p _) _ = Left (Located p "unterminated basic string")
eofToken (MlLstrContext p _) _ = Left (Located p "unterminated multi-line literal string")
eofToken (LstrContext   p _) _ = Left (Located p "unterminated literal string")
eofToken _                   t = Right (TokEOF <$ t, t)

failure :: String -> Action
failure err t _ = LexerError (err <$ t)

-- | Generate an error message given the current string being lexed.
mkError :: String -> String
mkError ""    = "unexpected end-of-input"
mkError ('\n':_) = "unexpected end-of-line"
mkError ('\r':'\n':_) = "unexpected end-of-line"
mkError (x:_)
    | isControl x = "control characters prohibited"
    | otherwise   = "unexpected " ++ show x
