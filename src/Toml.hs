{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Toml
Description : TOML parsing, printing, and codecs
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This is the high-level interface to the toml-parser library.
It enables parsing, printing, and conversion into and out of
application-specific representations.

This parser implements TOML 1.0.0 <https://toml.io/en/v1.0.0>
as carefully as possible.

-}
module Toml (

    -- * Types
    Table,
    Value,

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

import Data.Text (Text)
import Toml.Pretty (TomlDoc, DocClass(..), prettyToml, prettySemanticError, prettyMatchMessage, prettyLocated)
import Toml.Schema
import Toml.Semantics (Value, Value'(..), Table, Table'(..), semantics, forgetTableAnns)
import Toml.Syntax

-- | Parse a TOML formatted 'String' or report an error message.
parse' :: Text -> Either String (Table' Position)
parse' str =
    case parseRawToml str of
        Left e -> Left (prettyLocated e)
        Right exprs ->
            case semantics exprs of
                Left e -> Left (prettySemanticError e)
                Right tab -> Right tab

-- | Parse a TOML formatted 'String' or report an error message.
parse :: Text -> Either String Table
parse = fmap forgetTableAnns . parse'

-- | Use the 'FromValue' instance to decode a value from a TOML string.
decode :: FromValue a => Text -> Result String a
decode str =
    case parse' str of
        Left e -> Failure [e]
        Right tab ->
            case runMatcher (fromValue (Table' startPos tab)) of
                Failure es -> Failure (prettyMatchMessage <$> es)
                Success ws x -> Success (prettyMatchMessage <$> ws) x

-- | Use the 'ToTable' instance to encode a value to a TOML string.
encode :: ToTable a => a -> TomlDoc
encode = prettyToml . toTable
