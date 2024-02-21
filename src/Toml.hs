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

    -- * Errors
    prettyMatchMessage,
    prettySemanticError
    ) where

import Data.Text (Text)
import Text.Printf (printf)
import Toml.Pretty
import Toml.Schema
import Toml.Semantics
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

-- | Render a TOML decoding error as a human-readable string.
--
-- @since 1.3.0.0
prettyMatchMessage :: MatchMessage Position -> String
prettyMatchMessage (MatchMessage loc scope msg) = prefix ++ msg ++ " in " ++ path
    where
        prefix =
            case loc of
                Nothing -> ""
                Just l -> prettyPosition l ++ ": "
        path =
            case scope of
                [] -> "<top-level>"
                ScopeKey key : scope' -> shows (prettySimpleKey key) (foldr f "" scope')
                ScopeIndex i : scope' -> foldr f "" (ScopeIndex i : scope') -- should be impossible

        f (ScopeIndex i) = showChar '[' . shows i . showChar ']'
        f (ScopeKey key) = showChar '.' . shows (prettySimpleKey key)

-- | Render a semantic TOML error in a human-readable string.
--
-- @since 1.3.0.0
prettySemanticError :: SemanticError Position -> String
prettySemanticError (SemanticError a key kind) =
    printf "%s: key error: %s %s" (prettyPosition a) (show (prettySimpleKey key))
    case kind of
        AlreadyAssigned -> "is already assigned" :: String
        ClosedTable     -> "is a closed table"
        ImplicitlyTable -> "is already implicitly defined to be a table"
