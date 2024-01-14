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
    deriving ToValue   via (GenericToml Physical)
    deriving ToTable   via (GenericToml Physical)
    deriving FromValue via (GenericToml Physical)
@

-}
module Toml.Generic (
    GenericToml(GenericToml),
    ) where

import Data.Coerce (coerce)
import GHC.Generics (Generic(Rep))
import Toml.FromValue (FromValue(fromValue), parseTableFromValue)
import Toml.FromValue.Generic (GParseTable, genericParseTable)
import Toml.FromValue.Matcher (Matcher)
import Toml.ToValue (ToTable(toTable), ToValue(toValue), defaultTableToValue)
import Toml.ToValue.Generic (GToTable, genericToTable)
import Toml.Value (Value, Table)

-- | Helper type to use GHC's DerivingVia extension to derive
-- 'ToValue', 'FromValue'
newtype GenericToml a = GenericToml a

-- | Instance derived from 'ToTable' instance using 'defaultTableToValue'
instance ToTable (GenericToml a) => ToValue (GenericToml a) where
    toValue = defaultTableToValue
    {-# INLINE toValue #-}

-- | Instance derived using 'genericToTable'
instance (Generic a, GToTable (Rep a)) => ToTable (GenericToml a) where
    toTable = coerce (genericToTable :: a -> Table)
    {-# INLINE toTable #-}

-- | Instance derived using 'genericParseTable'
instance (Generic a, GParseTable (Rep a)) => FromValue (GenericToml a) where
    fromValue = coerce (parseTableFromValue genericParseTable :: Value -> Matcher a)
    {-# INLINE fromValue #-}
