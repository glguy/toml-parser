{-# Language DataKinds, InstanceSigs, ScopedTypeVariables, TypeOperators #-}
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
    -- * Record from table
    GParseTable(..),
    genericParseTable,

    -- * Product type from array 
    GFromArray(..),
    genericFromArray,
    ) where

import GHC.Generics

import Toml.FromValue.ParseTable (ParseTable)
import Toml.FromValue (FromValue, fromValue, optKey, reqKey)
import Toml.FromValue.Matcher
import Toml
import Data.Coerce

-- | Match a 'Table' using the field names in a record.
--
-- @since 1.2.0.0
genericParseTable :: (Generic a, GParseTable (Rep a)) => ParseTable a
genericParseTable = gParseTable (pure . to)
{-# INLINE genericParseTable #-}

-- | Match a 'Value' as an array positionally matching field fields
-- of a constructor to the elements of the array.
--
-- @since 1.3.2.0
genericFromArray :: (Generic a, GFromArray (Rep a)) => Value -> Matcher a
genericFromArray v =
 do xs <- fromValue v
    (xs', gen) <- gFromArray xs
    if null xs' then
        pure (to gen)
    else
        fail ("array " ++ show (length xs') ++ " elements too long")

-- gParseTable is written in continuation passing style because
-- it allows all the GHC.Generics constructors to inline into
-- a single location which allows the optimizer to optimize them
-- complete away.

-- | Supports conversion of TOML tables into record values using
-- field selector names as TOML keys.
--
-- @since 1.0.2.0
class GParseTable f where
    -- | Convert a value and apply the continuation to the result.
    gParseTable :: (f a -> ParseTable b) -> ParseTable b

-- | Ignores type constructor name
instance GParseTable f => GParseTable (D1 c f) where
    gParseTable f = gParseTable (f . M1)
    {-# INLINE gParseTable #-}

-- | Ignores value constructor name - only supports record constructors
instance GParseTable f => GParseTable (C1 ('MetaCons sym fix 'True) f) where
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

-- | Supports conversion of TOML arrays into product-type values.
--
-- @since 1.3.2.0
class GFromArray f where
    gFromArray :: [Value] -> Matcher ([Value], f a)

instance GFromArray f => GFromArray (M1 i c f) where
    gFromArray :: forall a. [Value] -> Matcher ([Value], M1 i c f a)
    gFromArray = coerce (gFromArray :: [Value] -> Matcher ([Value], f a))
    {-# INLINE gFromArray #-}

instance (GFromArray f, GFromArray g) => GFromArray (f :*: g) where
    gFromArray xs =
     do (xs1, x) <- gFromArray xs
        (xs2, y) <- gFromArray xs1
        pure (xs2, x :*: y)
    {-# INLINE gFromArray #-}

instance FromValue a => GFromArray (K1 i a) where
    gFromArray [] = fail "Array too short"
    gFromArray (x:xs) =
     do v <- fromValue x
        pure (xs, K1 v)
    {-# INLINE gFromArray #-}

-- | Uses no array elements
instance GFromArray U1 where
    gFromArray xs = pure (xs, U1)
    {-# INLINE gFromArray #-}
