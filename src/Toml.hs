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
module Toml (
    Value(..),
    parse,
    prettyToml,

    -- * Serialization
    decode,
    encode,
    ) where

import Data.Map (Map)
import Text.Printf (printf)
import Toml.FromValue (FromTable (fromTable))
import Toml.Lexer (scanTokens)
import Toml.Located (Located(Located))
import Toml.Parser (parseRawToml)
import Toml.Pretty (TomlDoc, prettyToken, prettyToml)
import Toml.Semantics (semantics)
import Toml.ToValue (ToTable (toTable))
import Toml.Value (Value(..))
import Toml.Position (Position(posColumn, posLine))
import Toml.Token (Token(TokError))
import Toml.Result (Result)

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String (Map String Value)
parse str =
    case parseRawToml (scanTokens str) of
        Left (Located p (TokError e)) ->
            Left (printf "%d:%d: lexical error: %s" (posLine p) (posColumn p) e)
        Left (Located p t) ->
            Left (printf "%d:%d: parse error: unexpected %s" (posLine p) (posColumn p) (prettyToken t))
        Right exprs -> semantics exprs

-- | Use the 'FromTable' instance to decode a value from a TOML string.
decode :: FromTable a => String -> Result a
decode = either fail fromTable . parse

-- | Use the 'ToTable' instance to encode a value to a TOML string.
encode :: ToTable a => a -> TomlDoc
encode = prettyToml . toTable
