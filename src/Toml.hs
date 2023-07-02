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

import Text.Printf (printf)
import Toml.FromValue (FromTable (fromTable), runMatcher, Result(..))
import Toml.Lexer (scanTokens, Token(TokError))
import Toml.Located (Located(Located))
import Toml.Parser (parseRawToml)
import Toml.Position (Position(posColumn, posLine))
import Toml.Pretty (TomlDoc, DocClass(..), prettyToken, prettyToml)
import Toml.Semantics (semantics)
import Toml.ToValue (ToTable (toTable))
import Toml.Value (Table, Value(..))

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String Table
parse str =
    case parseRawToml (scanTokens str) of
        Left (Located p (TokError e)) ->
            Left (printf "%d:%d: lexical error: %s" (posLine p) (posColumn p) e)
        Left (Located p t) ->
            Left (printf "%d:%d: parse error: unexpected %s" (posLine p) (posColumn p) (prettyToken t))
        Right exprs -> semantics exprs

-- | Use the 'FromTable' instance to decode a value from a TOML string.
decode :: FromTable a => String -> Result a
decode = either (Failure . pure) (runMatcher . fromTable) . parse

-- | Use the 'ToTable' instance to encode a value to a TOML string.
encode :: ToTable a => a -> TomlDoc
encode = prettyToml . toTable
