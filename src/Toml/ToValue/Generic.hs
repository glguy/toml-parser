{-|
Module      : Toml.ToValue.Matcher
Description : GHC.Generics derived table generation
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'genericToTable' to derive an instance of 'Toml.ToValue.ToTable'
using the field names of a record.

-}
module Toml.ToValue.Generic (
    GToTable(..),
    genericToTable,
    ) where

import Data.Map qualified as Map
import GHC.Generics
import Toml.Value (Table)
import Toml.ToValue (ToValue(..))

-- | Use a record's field names to generate a 'Table'
--
-- @since 1.0.2.0
genericToTable :: (Generic a, GToTable (Rep a)) => a -> Table
genericToTable = gToTable . from
{-# INLINE genericToTable #-}

-- | Supports conversion of product types with field selector names
-- to TOML values.
--
-- @since 1.0.2.0
class GToTable f where
    gToTable :: f a -> Table

-- | Ignores type constructor names
instance GToTable f => GToTable (D1 c f) where
    gToTable (M1 x) = gToTable x
    {-# INLINE gToTable #-}

-- | Ignores value constructor names
instance GToTable f => GToTable (C1 c f) where
    gToTable (M1 x) = gToTable x
    {-# INLINE gToTable #-}

instance (GToTable f, GToTable g) => GToTable (f :*: g) where
    gToTable (x :*: y) = gToTable x <> gToTable y
    {-# INLINE gToTable #-}

-- | Omits the key from the table on nothing, includes it on just
instance {-# OVERLAPS #-} (Selector s, ToValue a) => GToTable (S1 s (K1 i (Maybe a))) where
    gToTable s@(M1 (K1 Nothing)) = Map.empty
    gToTable s@(M1 (K1 (Just x))) = Map.singleton (selName s) (toValue x)
    {-# INLINE gToTable #-}

-- | Uses record selector name as table key
instance (Selector s, ToValue a) => GToTable (S1 s (K1 i a)) where
    gToTable s@(M1 (K1 x)) = Map.singleton (selName s) (toValue x)
    {-# INLINE gToTable #-}

-- | Emits empty table
instance GToTable U1 where
    gToTable _ = Map.empty
    {-# INLINE gToTable #-}

instance GToTable V1 where
    gToTable v = case v of {}
    {-# INLINE gToTable #-}
