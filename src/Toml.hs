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
    Value(..),

    -- * Parsing
    parse,

    -- * Printing
    prettyToml,
    DocClass(..),

    -- * Serialization
    decode,
    encode,
    Result(..),
    ) where

import Control.Monad ((<=<))
import Text.Printf (printf)
import Toml.FromValue (FromValue (fromValue), Result(..))
import Toml.FromValue.Matcher (runMatcher)
import Toml.Located (Located(Located))
import Toml.Parser (parseRawToml)
import Toml.Position (Position(posColumn, posLine))
import Toml.Pretty (TomlDoc, DocClass(..), prettyToken, prettyToml, prettySimpleKey)
import Toml.Semantics (SemanticError(..), SemanticErrorKind(..), semantics)
import Toml.ToValue (ToTable (toTable))
import Toml.Value (Table, Value(..))
import Toml.Position(Position(..))

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String Table
parse str =
    case parseRawToml str of
        Left (Located p e) -> Left (printf "%d:%d: %s" (posLine p) (posColumn p) e)
        Right exprs ->
            case semantics exprs of
                Left (Located p (SemanticError k e)) ->
                    Left (printf "%d:%d: key error: %s %s" (posLine p) (posColumn p) (show (prettySimpleKey k)) (prettySemanticError e))
                Right tab -> Right tab
    where
        prettySemanticError AlreadyAssigned = "is already assigned"
        prettySemanticError ClosedTable     = "is a closed table"
        prettySemanticError ImplicitlyTable = "is already implicitly defined to be a table"

-- | Use the 'FromValue' instance to decode a value from a TOML string.
decode :: FromValue a => String -> Result a
decode = either (Failure . pure) (runMatcher . fromValue . Table) . parse

-- | Use the 'ToTable' instance to encode a value to a TOML string.
encode :: ToTable a => a -> TomlDoc
encode = prettyToml . toTable
