{-|
Module      : Toml.Schema
Description : Infrastructure for converting between TOML and application values
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Toml.Schema (
    -- * FromValue
    FromValue(..),

    -- ** Matcher
    Matcher,
    runMatcher,
    runMatcherFatalWarn,
    runMatcherIgnoreWarn,
    Result(..),
    MatchMessage(..),
    Scope(..),
    parseTableFromValue,
    parseTable,
    getScope,
    warn,
    warnAt,
    failAt,
    getTable,
    setTable,

    -- ** Tables
    ParseTable,
    reqKey,
    optKey,
    reqKeyOf,
    optKeyOf,
    pickKey,
    KeyAlt(..),
    warnTable,
    warnTableAt,
    failTableAt,
    liftMatcher,

    -- * ToValue
    ToValue(..),
    ToTable(..),

    table,
    (.=),
    defaultTableToValue,

    -- * Types
    Value, Value'(..),
    Table, Table'(..),

    -- * Generics
    GenericTomlArray(..),
    GenericTomlTable(..),
    genericFromTable,
    genericFromArray,
    genericToArray,
    genericToTable,

) where

import Toml.Schema.FromValue
import Toml.Schema.Generic
import Toml.Schema.ToValue
import Toml.Semantics
