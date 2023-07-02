{-|
Module      : Toml.FromValue.Generic
Description : GHC.Generics derived table parsing
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'genericFromTable' to derive an instance of 'Toml.FromValue.FromTable'
using the field names of a record.

-}
module Toml.FromValue.Generic (
    GParseTable(..),
    genericFromTable,
    ) where

import Data.Map qualified as Map
import GHC.Generics
import Toml.FromValue (FromValue(..), ParseTable, optKey, reqKey, runParseTable)
import Toml.FromValue.Matcher (Matcher)
import Toml.Value (Table)

-- | Match a 'Table' using the field names in a record.
--
-- @since 1.0.2.0
genericFromTable :: (Generic a, GParseTable (Rep a)) => Table -> Matcher a
genericFromTable t = runParseTable (gParseTable (pure . to)) t
{-# INLINE genericFromTable #-}

-- gParseTable is written in continuation passing style because
-- it allows all the GHC.Generics constructors to inline into
-- a single location which allows the optimizer to optimize them
-- complete away.

-- | Supports conversion of product types with field selector names to
-- TOML values.
--
-- @since 1.0.2.0
class GParseTable f where
    -- | Convert a value and apply the continuation to the result.
    gParseTable :: (f a -> ParseTable b) -> ParseTable b

-- | Ignores type constructor name
instance GParseTable f => GParseTable (D1 c f) where
    gParseTable f = gParseTable (f . M1)
    {-# INLINE gParseTable #-}

-- | Ignores value constructor name
instance GParseTable f => GParseTable (C1 c f) where
    gParseTable f = gParseTable (f . M1)
    {-# INLINE gParseTable #-}

instance (GParseTable f, GParseTable g) => GParseTable (f :*: g) where
    gParseTable f = gParseTable \x -> gParseTable \y -> f (x :*: y)
    {-# INLINE gParseTable #-}

-- | Omits the key from the table on nothing, includes it on just
instance {-# OVERLAPS #-} (Selector s, FromValue a) => GParseTable (S1 s (K1 i (Maybe a))) where
    gParseTable f = f . M1 . K1 =<< optKey (selName (undefined :: S1 s [] ()))
    {-# INLINE gParseTable #-}

-- | Uses record selector name as table key
instance (Selector s, FromValue a) => GParseTable (S1 s (K1 i a)) where
    gParseTable f = f . M1 . K1 =<< reqKey (selName (undefined :: S1 s [] ()))
    {-# INLINE gParseTable #-}

-- | Emits empty table
instance GParseTable U1 where
    gParseTable f = f U1
    {-# INLINE gParseTable #-}
