{-|
Module      : Toml.FromValue.Generic
Description : GHC.Generics derived table parsing
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'genericParseTable' to derive a 'ParseTable' using the field names
of a record. This can be combined with 'Toml.FromValue.parseTableFromValue'
to derive a 'Toml.FromValue.FromValue' instance.

-}
module Toml.FromValue.Generic (
    GParseTable(..),
    genericParseTable,
    ) where

import GHC.Generics
import Toml.FromValue.ParseTable (ParseTable, runParseTable)
import Toml.FromValue.Matcher (Matcher)
import Toml.FromValue (FromValue, fromValue, optKey, reqKey)
import Toml.Value (Table, Value(Table))

-- | Match a 'Table' using the field names in a record.
--
-- @since 1.2.0.0
genericParseTable :: (Generic a, GParseTable (Rep a)) => ParseTable a
genericParseTable = gParseTable (pure . to)
{-# INLINE genericParseTable #-}

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

-- | Matches left then right component
instance (GParseTable f, GParseTable g) => GParseTable (f :*: g) where
    gParseTable f = gParseTable \x -> gParseTable \y -> f (x :*: y)
    {-# INLINE gParseTable #-}

-- | Omits the key from the table on nothing, includes it on just
instance {-# OVERLAPS #-} (Selector s, FromValue a) => GParseTable (S1 s (K1 i (Maybe a))) where
    gParseTable f = f . M1 . K1 =<< optKey (selName (M1 [] :: S1 s [] ()))
    {-# INLINE gParseTable #-}

-- | Uses record selector name as table key
instance (Selector s, FromValue a) => GParseTable (S1 s (K1 i a)) where
    gParseTable f = f . M1 . K1 =<< reqKey (selName (M1 [] :: S1 s [] ()))
    {-# INLINE gParseTable #-}

-- | Emits empty table
instance GParseTable U1 where
    gParseTable f = f U1
    {-# INLINE gParseTable #-}
