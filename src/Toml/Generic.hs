{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
{-|
Module      : Toml.Generic
Description : Integration with DerivingVia extension
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

This module makes it possible to easily derive the TOML classes
using the @DerivingVia@ extension.

For example:

@
data Physical = Physical {
    color :: String,
    shape :: String
    }
    deriving (Eq, Show, Generic)
    deriving (ToTable, ToValue, FromValue) via GenericTomlTable Physical
@

These derived instances would allow you to match TOML
@{color="red", shape="round"}@ to value @Physical "red" "round"@.

@
data Coord = Coord Int Int
    deriving (Eq, Show, Generic)
    deriving (ToValue, FromValue) via GenericTomlArray Physical
@

These derived instances would allow you to match TOML @[1,2]@ to value @Coord 1 2@.

-}
module Toml.Generic (
    GenericTomlTable(GenericTomlTable),
    GenericTomlArray(GenericTomlArray),
    ) where

import Data.Coerce (coerce)
import GHC.Generics (Generic(Rep))
import Toml.FromValue (FromValue(fromValue), parseTableFromValue)
import Toml.FromValue.Generic (GParseTable, GFromArray, genericParseTable, genericFromArray)
import Toml.FromValue.Matcher (Matcher)
import Toml.ToValue (ToTable(toTable), ToValue(toValue), defaultTableToValue)
import Toml.ToValue.Generic (GToTable, GToArray, genericToTable, genericToArray)
import Toml.Value (Value, Table)

-- | Helper type to use GHC's DerivingVia extension to derive
-- 'ToValue', 'ToTable', 'FromValue' for records.
--
-- @since 1.3.2.0
newtype GenericTomlTable a = GenericTomlTable a

-- | Instance derived from 'ToTable' instance using 'defaultTableToValue'
instance (Generic a, GToTable (Rep a)) => ToValue (GenericTomlTable a) where
    toValue = defaultTableToValue
    {-# INLINE toValue #-}

-- | Instance derived using 'genericToTable'
instance (Generic a, GToTable (Rep a)) => ToTable (GenericTomlTable a) where
    toTable = coerce (genericToTable :: a -> Table)
    {-# INLINE toTable #-}

-- | Instance derived using 'genericParseTable'
instance (Generic a, GParseTable (Rep a)) => FromValue (GenericTomlTable a) where
    fromValue = coerce (parseTableFromValue genericParseTable :: Value -> Matcher a)
    {-# INLINE fromValue #-}

-- | Helper type to use GHC's DerivingVia extension to derive
-- 'ToValue', 'ToTable', 'FromValue' for any product type.
--
-- @since 1.3.2.0
newtype GenericTomlArray a = GenericTomlArray a

-- | Instance derived using 'genericToArray'
instance (Generic a, GToArray (Rep a)) => ToValue (GenericTomlArray a) where
    toValue = coerce (genericToArray :: a -> Value)
    {-# INLINE toValue #-}

-- | Instance derived using 'genericFromArray'
instance (Generic a, GFromArray (Rep a)) => FromValue (GenericTomlArray a) where
    fromValue = coerce (genericFromArray :: Value -> Matcher a)
    {-# INLINE fromValue #-}