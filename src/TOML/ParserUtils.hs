{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : TOML.ParserUtils
Description : /Internal:/ Parser support operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module is separate from the Parser.y input to Happy
to segregate the automatically generated code from the
hand written code. The automatically generated code
causes lots of warnings which mask the interesting warnings.
-}
module TOML.ParserUtils
  (
  -- * inf + nan
    getInfValue
  , getInfKey
  , getNanValue
  , getNanKey

  -- * Errors
  , errorP
  , unterminated
  ) where

import           Data.Text (Text)

import           TOML.Errors
import           TOML.Located
import           TOML.Tokens
import           TOML.Value

getInfValue :: Located Token -> Value
getInfValue located =
  case located of
    Located _ (InfToken str)
      | "inf" <- str -> Double inf
      | "+inf" <- str -> Double inf
      | "-inf" <- str -> Double (-inf)
    _ -> error $ "Unexpected token: " ++ show located
  where
    inf = read "Infinity"

getInfKey :: Located Token -> Either TOMLError Text
getInfKey located =
  case located of
    Located _ (InfToken str)
      | "inf" <- str -> Right str
      | "+inf" <- str -> Left $ Unexpected located
      | "-inf" <- str -> Right str
    _ -> error $ "Unexpected token: " ++ show located

getNanValue :: Located Token -> Value
getNanValue located =
  case located of
    Located _ (NanToken str)
      | "nan" <- str -> Double nan
      | "+nan" <- str -> Double nan
      | "-nan" <- str -> Double (-nan)
    _ -> error $ "Unexpected token: " ++ show located
  where
    nan = read "NaN"

getNanKey :: Located Token -> Either TOMLError Text
getNanKey located =
  case located of
    Located _ (NanToken str)
      | "nan" <- str -> Right str
      | "+nan" <- str -> Left $ Unexpected located
      | "-nan" <- str -> Right str
    _ -> error $ "Unexpected token: " ++ show located

-- | This operation is called by happy when no production matches the
-- current token list.
errorP :: [Located Token] {- ^ nonempty remainig tokens -} -> Either TOMLError a
errorP = Left . Unexpected . head

-- | Abort the parse with an error indicating that the given token was unmatched.
unterminated :: Located Token -> Either TOMLError a
unterminated = Left . Unterminated
