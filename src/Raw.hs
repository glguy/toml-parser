{-|
Module      : Raw
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
module Raw where

import Data.List.NonEmpty (NonEmpty)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)

type Key = NonEmpty String

data Expr
    = KeyValExpr     Int Key Val
    | TableExpr      Int Key
    | ArrayTableExpr Int Key
    deriving Show

data Val
    = ValInteger Integer
    | ValFloat Double
    | ValArray [Val]
    | ValTable [(Key, Val)]
    | ValBool Bool
    | ValString String
    | ValTimeOfDay TimeOfDay
    | ValZonedTime ZonedTime
    | ValLocalTime LocalTime
    | ValDay Day
    deriving Show

data SectionKind = TableKind | ArrayTableKind
    deriving Show