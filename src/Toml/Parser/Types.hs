{-|
Module      : Toml.Raw
Description : Raw expressions from a parsed TOML file
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a raw representation of TOML files as
a list of table definitions and key-value assignments.

These values use the raw dotted keys and have no detection
for overlapping assignments.

Further processing will happen in the "Semantics" module.

-}
module Toml.Parser.Types (
    Key,
    Expr(..),
    Val(..),
    SectionKind(..),
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)

-- | Non-empty sequence of dotted simple keys
type Key a = NonEmpty (a, String)

-- | Headers and assignments corresponding to lines of a TOML file
data Expr a
    = KeyValExpr     (Key a) (Val a) -- ^ key value assignment: @key = value@
    | TableExpr      (Key a)         -- ^ table: @[key]@
    | ArrayTableExpr (Key a)         -- ^ array of tables: @[[key]]@
    deriving (Read, Show)


-- | Unvalidated TOML values. Table are represented as a list of
-- assignments rather than as resolved maps.
data Val a
    = ValInteger   a Integer
    | ValFloat     a Double
    | ValArray     a [Val a]
    | ValTable     a [(Key a, Val a)]
    | ValBool      a Bool
    | ValString    a String
    | ValTimeOfDay a TimeOfDay
    | ValZonedTime a ZonedTime
    | ValLocalTime a LocalTime
    | ValDay       a Day
    deriving (Read, Show)

-- | Kinds of table headers
data SectionKind
    = TableKind -- ^ [table]
    | ArrayTableKind -- ^ [[array of tables]]
    deriving (Read, Show, Eq)
