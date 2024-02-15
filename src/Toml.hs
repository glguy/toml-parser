{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Toml
Description : TOML parsing, printing, and codecs
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This is the high-level interface to the toml-parser library.
It enables parsing, printing, and coversion into and out of
application-specific representations.

This parser implements TOML 1.0.0 <https://toml.io/en/v1.0.0>
as carefully as possible.

-}
module Toml (

    -- * Types
    Table,
    Value,
    pattern Integer, pattern Float, pattern String, pattern Bool,
    pattern ZonedTime, pattern Day, pattern LocalTime, pattern TimeOfDay,
    pattern Array, pattern Table,

    -- * Located types
    Located(..),
    Position(..),
    Table'(..),
    Value'(..),

    -- * Parsing
    decode,
    Result(..),
    parse,
    parse',

    -- * Printing
    encode,
    prettyToml,
    DocClass(..),
    ) where

import Toml.FromValue (FromValue (fromValue), Result(..))
import Toml.FromValue.Matcher (runMatcher)
import Toml.Located (Located(..))
import Toml.Parser (parseRawToml)
import Toml.Position (Position(..), startPos)
import Toml.Pretty (TomlDoc, DocClass(..), prettyToml, prettySemanticError, prettyMatchMessage, prettyLocated, prettyPosition)
import Toml.Semantics (semantics)
import Toml.ToValue (ToTable (toTable))
import Toml.Value

-- | Parse a TOML formatted 'String' or report an error message.
parse' :: String -> Either String (Table' Position)
parse' str =
    case parseRawToml str of
        Left e -> Left (prettyLocated e)
        Right exprs ->
            case semantics exprs of
                Left e -> Left (prettySemanticError (fmap prettyPosition e))
                Right tab -> Right tab

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String Table
parse = fmap forgetTableAnns . parse'

-- | Use the 'FromValue' instance to decode a value from a TOML string.
decode :: FromValue a => String -> Result String a
decode str =
    case parse' str of
        Left e -> Failure [e]
        Right tab ->
            case runMatcher (fromValue (Table' startPos tab)) of
                Failure es -> Failure (prettyMatchMessage . fmap prettyPosition <$> es)
                Success ws x -> Success (prettyMatchMessage . fmap prettyPosition <$> ws) x

-- | Use the 'ToTable' instance to encode a value to a TOML string.
encode :: ToTable a => a -> TomlDoc
encode = prettyToml . toTable
