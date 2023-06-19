{-|
Module      : Toml.LexerUtils
Description : Wrapper and actions for generated lexer
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a custom engine for the Alex generated
lexer. This lexer drive provides nested states, unicode support,
and file location tracking.

-}
module Toml.LexerUtils where

import Control.Monad.Trans.State (State, modify, state)
import Data.Char (ord, isAscii)
import Data.Foldable (asum)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)

import Toml.Located (Located(..))
import Toml.Position (move)
import Toml.Token (Token(..))

type M a = State [Context] a

type Action = String -> M Token

data Context
  = ListContext  -- ^ processing an inline list, lex values
  | TableContext -- ^ processing an inline table, don't lex values
  | ValueContext -- ^ processing after an equals, lex one value
  deriving Show

pushContext :: Context -> M ()
pushContext cxt = modify \st ->
  case st of
    ValueContext : st' -> cxt : st'
    _                  -> cxt : st

equals :: Action
equals _ = TokEquals <$ pushContext ValueContext

enterList :: Action
enterList _ = TokSquareO <$ pushContext ListContext

enterTable :: Action
enterTable _ = TokCurlyO <$ pushContext TableContext

exitTable :: Action
exitTable _ = state \case
  TableContext : st -> (TokCurlyC             , st)
  st                -> (TokError "Unmatched }", st)

exitList :: Action
exitList _ = state \case
  ListContext : st -> (TokSquareC            , st)
  []               -> (TokSquareC            , [])
  st               -> (TokError "Unmatched ]", st)

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
    Nothing -> TokError ("Malformed " ++ description)
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
