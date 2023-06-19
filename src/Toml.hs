{-|
Module      : Toml
Description : TOML parser
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module parses TOML into semantically meaningful values.

This parser implements TOML 1.0.0 <https://toml.io/en/v1.0.0>
as carefully as possible.

-}
module Toml (parse, Value(..)) where

import Data.Map (Map)
import Toml.Lexer (scanTokens)
import Toml.Located (Located(locPosition, locThing))
import Toml.Parser (parseRawToml)
import Toml.Pretty (prettyPosition)
import Toml.Semantics (semantics)
import Toml.Token (prettyToken)
import Toml.Value (Value(..))

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String (Map String Value)
parse str =
    case parseRawToml (scanTokens str) of
        Left le ->
            Left ("Unexpected " ++ prettyToken (locThing le) ++ " at " ++ prettyPosition (locPosition le))
        Right exprs -> semantics exprs
