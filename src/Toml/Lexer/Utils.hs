{-|
Module      : Toml.Lexer.Utils
Description : Wrapper and actions for generated lexer
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a custom engine for the Alex generated
lexer. This lexer drive provides nested states, unicode support,
and file location tracking.

-}
module Toml.Lexer.Utils (

    -- * Types
    Action,
    Context(..),

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

    strFrag,
    startMlStr,
    startStr,
    endStr,
    unicodeEscape,

    -- * Alex extension points
    AlexInput,
    alexGetByte,

    ) where

import Control.Monad.Trans.State.Strict (State, modify, state)
import Data.Char (ord, chr, isAscii)
import Data.Foldable (asum)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Numeric (readHex)

import Toml.Located (Located(..))
import Toml.Position (move, Position)
import Toml.Lexer.Token (Token(..))

type Action = Located String -> State [Context] [Located Token]

data Context
  = ListContext Position -- ^ processing an inline list, lex values
  | TableContext Position -- ^ processing an inline table, don't lex values
  | ValueContext -- ^ processing after an equals, lex one value
  | MlStrContext Position [String]
  | StrContext   Position [String]
  deriving Show

strFrag :: Action
strFrag s = state \case
  StrContext   p acc : st -> ([], StrContext   p (locThing s:acc) : st)
  MlStrContext p acc : st -> ([], MlStrContext p (locThing s:acc) : st)
  _ -> error "strFrag: panic"

endStr :: Action
endStr x = state \case
    StrContext   p acc : st -> ([Located p $ TokString   (concat (reverse (locThing x : acc)))], st)
    MlStrContext p acc : st -> ([Located p $ TokMlString (concat (reverse (locThing x : acc)))], st)
    _                       -> error "endStr: panic"

startStr :: Action
startStr t = state \case
  ValueContext : st -> ([], StrContext (locPosition t) [] : st)
  st -> ([], StrContext (locPosition t) [] : st)

startMlStr :: Action
startMlStr t = state \case
  ValueContext : st -> ([], MlStrContext (locPosition t) [] : st)
  st -> ([], MlStrContext (locPosition t) [] : st)

unicodeEscape :: Action
unicodeEscape (Located p lexeme) =
  case readHex (drop 2 lexeme) of
    [(n,_)] | 0xd800 <= n, n < 0xe000 -> pure [Located p $ TokError "non-scalar unicode escape"]
      | n >= 0x110000 -> pure [Located p $ TokError "unicode escape too large"]
      | otherwise -> strFrag (Located p [chr n])
    _ -> error "unicodeEscape: panic"

equals :: Action
equals t = state \case
  st -> ([TokEquals <$ t], ValueContext : st)

squareO :: Action
squareO t = state \case
  ValueContext  : st -> ([TokSquareO <$ t], ListContext (locPosition t) : st)
  ListContext p : st -> ([TokSquareO <$ t], ListContext (locPosition t): ListContext p : st)
  st                 -> ([TokSquareO <$ t], st)

squareC :: Action
squareC t = state \case
  ListContext _ : st -> ([TokSquareC <$ t], st)
  st                 -> ([TokSquareC <$ t], st)

curlyO :: Action
curlyO t = state \case
  ValueContext  : st -> ([TokCurlyO <$ t], TableContext (locPosition t) : st)
  ListContext p : st -> ([TokCurlyO <$ t], TableContext (locPosition t) : ListContext p : st)
  st                 -> ([TokCurlyO <$ t], st)

curlyC :: Action
curlyC t = state \case
  TableContext _ : st -> ([TokCurlyC <$ t], st)
  st                  -> ([TokCurlyC <$ t], st)


token_ :: Token -> Action
token_ t x = pure [t <$ x]

token :: (String -> Token) -> Action
token f x = pure [f <$> x]

value_ :: Token -> Action
value_ t x = emitValue (t <$ x)

value :: (String -> Token) -> Action
value f x = emitValue (f <$> x)

emitValue :: Located Token -> State [Context] [Located Token]
emitValue v = state \st ->
  case st of
    ValueContext:st' -> ([v], st')
    _                -> ([v], st )

timeValue :: ParseTime a => String -> [String] -> (a -> Token) -> Action
timeValue description patterns constructor = value \str ->
  case asum [parseTimeM False defaultTimeLocale pattern str | pattern <- patterns] of
    Nothing -> TokError ("malformed " ++ description)
    Just t  -> constructor t

type AlexInput = Located String

alexGetByte :: AlexInput -> Maybe (Int, AlexInput)
alexGetByte Located { locPosition = p, locThing = str } =
  case str of
    "" -> Nothing
    x:xs
      | x == '\1' -> Just (0,     rest)
      | isAscii x -> Just (ord x, rest)
      | otherwise -> Just (1,     rest)
      where
        rest = Located { locPosition = move x p, locThing = xs }
