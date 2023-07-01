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
    M, Action,
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

    -- * Alex extension points
    AlexInput,
    alexGetByte,

    ) where

import Control.Monad.Trans.State.Strict (State, modify, state)
import Data.Char (ord, isAscii)
import Data.Foldable (asum)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)

import Toml.Located (Located(..))
import Toml.Position (move)
import Toml.Lexer.Token (Token(..))

type M a = State [Context] a

type Action = String -> M Token

data Context
  = ListContext  -- ^ processing an inline list, lex values
  | TableContext -- ^ processing an inline table, don't lex values
  | ValueContext -- ^ processing after an equals, lex one value
  deriving Show

equals :: Action
equals _ = state \case
  st -> (TokEquals, ValueContext : st)

squareO :: Action
squareO _ = state \case
  ValueContext : st -> (TokSquareO, ListContext : st)
  ListContext  : st -> (TokSquareO, ListContext : ListContext : st)
  st                -> (TokSquareO, st)

squareC :: Action
squareC _ = state \case
  ListContext : st -> (TokSquareC, st)
  st               -> (TokSquareC, st)

curlyO :: Action
curlyO _ = state \case
  ValueContext : st -> (TokCurlyO, TableContext : st)
  ListContext  : st -> (TokCurlyO, TableContext : ListContext : st)
  st                -> (TokCurlyO, st)

curlyC :: Action
curlyC _ = state \case
  TableContext : st -> (TokCurlyC, st)
  st                -> (TokCurlyC, st)


token_ :: Token -> Action
token_ t _ = pure t

token :: (String -> Token) -> Action
token f x = pure (f x)

value_ :: Token -> Action
value_ t _ = emitValue t

value :: (String -> Token) -> Action
value f x = emitValue (f x)

emitValue :: a -> M a
emitValue v = state \st ->
  case st of
    ValueContext:st' -> (v, st')
    _                -> (v, st )

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
