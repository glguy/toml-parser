{-|
Module      : TOML
Description : Parser for the TOML configuration language
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module TOML
  ( Located(..)
  , Token(..)
  , TOMLError(..)
  , LexerError(..)
  , Value(..)
  , parseTOML
  ) where

import Components
import Lexer
import Parser
import Tokens
import Value
import Errors

import Control.Monad
import Data.Text (Text)

parseTOML :: Text -> Either TOMLError [(Text,Value)]
parseTOML = mapLeft OverlappingKey . componentsToTable
        <=< parseComponents . scanTokens
  where
    mapLeft f (Left  e) = Left (f e)
    mapLeft _ (Right x) = Right x
