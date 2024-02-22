{-|
Module      : Toml.Syntax
Description : Parsing and lexing for TOML syntax
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

These are the low-level processing functions for transforming
concrete TOML syntax into abstract TOML syntax. This module
does not do any semantic validation of the parsed TOML.

@since 2.0.0.0

-}
module Toml.Syntax (
    -- * Parsing
    parseRawToml,
    Key,
    Expr(..),
    Val(..),

    -- * Lexing
    scanToken,
    Context(..),
    Token(..),

    -- * Locations
    Located(..),
    Position(..),
    startPos,
) where

import Toml.Syntax.Lexer
import Toml.Syntax.Parser
import Toml.Syntax.Position
