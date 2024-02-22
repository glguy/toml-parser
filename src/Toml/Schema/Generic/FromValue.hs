{-# Language DataKinds, InstanceSigs, ScopedTypeVariables, TypeOperators #-}
{-|
Module      : Toml.Schema.Generic.FromValue
Description : GHC.Generics derived table parsing
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Generic implementations of matching tables and arrays.

-}
module Toml.Schema.Generic.FromValue (
    -- * Record from table
    GParseTable(..),
    genericParseTable,
    genericFromTable,

    -- * Product type from array
    GFromArray(..),
    genericFromArray,
    ) where

import Control.Monad.Trans.State (StateT(..))
import Data.Coerce (coerce)
import Data.Text qualified as Text
import GHC.Generics
import Toml.Schema.FromValue (FromValue, fromValue, optKey, reqKey, parseTableFromValue, typeError)
import Toml.Schema.Matcher (Matcher, failAt)
import Toml.Schema.ParseTable (ParseTable)
import Toml.Semantics (Value'(List'))

-- | Match a 'Toml.Semantics.Table'' using the field names in a record.
--
-- @since 1.2.0.0
genericParseTable :: (Generic a, GParseTable (Rep a)) => ParseTable l a
genericParseTable = to <$> gParseTable
{-# INLINE genericParseTable #-}

-- | Implementation of 'fromValue' using 'genericParseTable' to derive
-- a match from the record field names of the target type.
genericFromTable :: (Generic a, GParseTable (Rep a)) => Value' l -> Matcher l a
genericFromTable = parseTableFromValue genericParseTable
{-# INLINE genericFromTable #-}

-- | Match a 'Toml.Semantics.Value'' as an array positionally matching field fields
-- of a constructor to the elements of the array.
--
-- @since 1.3.2.0
genericFromArray :: (Generic a, GFromArray (Rep a)) => Value' l -> Matcher l a
genericFromArray (List' a xs) =
 do (gen, xs') <- runStateT gFromArray xs
    if null xs' then
        pure (to gen)
    else
        failAt a ("array " ++ show (length xs') ++ " elements too long")
genericFromArray v = typeError "array" v

{-# INLINE genericFromArray #-}

-- 'gParseTable' is written in continuation passing style because
-- it allows all the "GHC.Generics" constructors to inline into
-- a single location which allows the optimizer to optimize them
-- complete away.

-- | Supports conversion of TOML tables into record values using
-- field selector names as TOML keys.
--
-- @since 1.0.2.0
class GParseTable f where
    -- | Convert a value and apply the continuation to the result.
    gParseTable :: ParseTable l (f a)

-- | Ignores type constructor name
instance GParseTable f => GParseTable (D1 c f) where
    gParseTable = M1 <$>  gParseTable
    {-# INLINE gParseTable #-}

-- | Ignores value constructor name - only supports record constructors
instance GParseTable f => GParseTable (C1 ('MetaCons sym fix 'True) f) where
    gParseTable = M1 <$> gParseTable
    {-# INLINE gParseTable #-}

-- | Matches left then right component
instance (GParseTable f, GParseTable g) => GParseTable (f :*: g) where
    gParseTable =
     do x <- gParseTable
        y <- gParseTable
        pure (x :*: y)
    {-# INLINE gParseTable #-}

-- | Omits the key from the table on nothing, includes it on just
instance {-# OVERLAPS #-} (Selector s, FromValue a) => GParseTable (S1 s (K1 i (Maybe a))) where
    gParseTable =
     do x <- optKey (Text.pack (selName (M1 [] :: S1 s [] ())))
        pure (M1 (K1 x))
    {-# INLINE gParseTable #-}

-- | Uses record selector name as table key
instance (Selector s, FromValue a) => GParseTable (S1 s (K1 i a)) where
    gParseTable =
     do x <- reqKey (Text.pack (selName (M1 [] :: S1 s [] ())))
        pure (M1 (K1 x))
    {-# INLINE gParseTable #-}

-- | Emits empty table
instance GParseTable U1 where
    gParseTable = pure U1
    {-# INLINE gParseTable #-}

-- | Supports conversion of TOML arrays into product-type values.
--
-- @since 1.3.2.0
class GFromArray f where
    gFromArray :: StateT [Value' l] (Matcher l) (f a)

instance GFromArray f => GFromArray (M1 i c f) where
    gFromArray :: forall a l. StateT [Value' l] (Matcher l) (M1 i c f a)
    gFromArray = coerce (gFromArray :: StateT [Value' l] (Matcher l) (f a))
    {-# INLINE gFromArray #-}

instance (GFromArray f, GFromArray g) => GFromArray (f :*: g) where
    gFromArray =
     do x <- gFromArray
        y <- gFromArray
        pure (x :*: y)
    {-# INLINE gFromArray #-}

instance FromValue a => GFromArray (K1 i a) where
    gFromArray = StateT \case
        [] -> fail "array too short"
        x:xs -> (\v -> (K1 v, xs)) <$> fromValue x
    {-# INLINE gFromArray #-}

-- | Uses no array elements
instance GFromArray U1 where
    gFromArray = pure U1
    {-# INLINE gFromArray #-}
