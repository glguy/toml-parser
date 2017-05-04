{-# Language Safe #-}
{-|
Module      : TOML
Description : Parser for the TOML configuration language
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Parser for the TOML file format: <https://github.com/toml-lang/toml>
-}
module TOML
  (
  -- * Parsing
    parseTOML

  -- * Values
  , Value(..)

  -- * Error information
  , TOMLError(..)
  , LexerError(..)
  , Located(..)
  , Position(..)
  , Token(..)
  ) where

import Control.Monad
import Data.Text (Text)

import TOML.Components
import TOML.Errors
import TOML.Lexer
import TOML.Located
import TOML.Parser
import TOML.Tokens
import TOML.Value

-- | Parse the given TOML file. Returns the top-level table or an error.
parseTOML :: Text -> Either TOMLError [(Text,Value)]
parseTOML = mapLeft OverlappingKey . componentsToTable
        <=< parseComponents . scanTokens
  where
    mapLeft f (Left  e) = Left (f e)
    mapLeft _ (Right x) = Right x
