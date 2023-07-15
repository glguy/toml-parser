{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Toml.ToValue
Description : Automation for converting application values to TOML.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

The 'ToValue' class provides a conversion function from
application-specific to TOML values.

Because the top-level TOML document is always a table,
the 'ToTable' class is for types that specifically support
conversion from a 'Table'.

"Toml.ToValue.Generic" can be used to derive instances of 'ToTable'
automatically for record types.

-}
module Toml.ToValue (
    ToValue(..),

    -- * Table construction
    ToTable(..),
    defaultTableToValue,
    table,
    (.=),
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.Value (Value(..), Table)
import Data.Ratio (Ratio)

-- | Build a 'Table' from a list of key-value pairs.
--
-- Use '.=' for a convenient way to build the pairs.
table :: [(String, Value)] -> Value
table = Table . Map.fromList

-- | Convenience function for building key-value pairs while
-- constructing a 'Table'.
--
-- @'table' [a '.=' b, c '.=' d]@
(.=) :: ToValue a => String -> a -> (String, Value)
k .= v = (k, toValue v)

-- | Class for types that can be embedded into 'Value'
class ToValue a where

    -- | Embed a single thing into a TOML value.
    toValue :: a -> Value

    -- | Helper for converting a list of things into a value. This is typically
    -- left to be defined by its default implementation and exists to help define
    -- the encoding for TOML arrays.
    toValueList :: [a] -> Value
    toValueList = Array . map toValue

-- | Class for things that can be embedded into a TOML table.
--
-- Implement this for things that embed into a 'Table' and then
-- the 'ToValue' instance can be derived with 'defaultTableToValue'.
class ToValue a => ToTable a where

    -- | Convert a single value into a table
    toTable :: a -> Table

-- | @since 1.0.1.0
instance (k ~ String, ToValue v) => ToTable (Map k v) where
    toTable m = Map.fromList [(k, toValue v) | (k,v) <- Map.assocs m]

-- | @since 1.0.1.0
instance (k ~ String, ToValue v) => ToValue (Map k v) where
    toValue = defaultTableToValue

-- | Convenience function for building 'ToValue' instances.
defaultTableToValue :: ToTable a => a -> Value
defaultTableToValue = Table . toTable

-- | Identity function
instance ToValue Value where
    toValue = id

-- | Single characters are encoded as singleton strings. Lists of characters
-- are encoded as a single string value.
instance ToValue Char where
    toValue x = String [x]
    toValueList = String

-- | Encodes as string literal
--
-- @since 1.2.1.0
instance ToValue Data.Text.Text where
    toValue = toValue . Data.Text.unpack

-- | Encodes as string literal
--
-- @since 1.2.1.0
instance ToValue Data.Text.Lazy.Text where
    toValue = toValue . Data.Text.Lazy.unpack

-- | This instance defers to the list element's 'toValueList' implementation.
instance ToValue a => ToValue [a] where
    toValue = toValueList

-- | Converts to a 'Double'. This can overflow to infinity.
--
-- @since 1.3.0.0
instance Integral a => ToValue (Ratio a) where
    toValue = Float . realToFrac

instance ToValue Double    where toValue = Float
instance ToValue Float     where toValue = Float . realToFrac
instance ToValue Bool      where toValue = Bool
instance ToValue TimeOfDay where toValue = TimeOfDay
instance ToValue LocalTime where toValue = LocalTime
instance ToValue ZonedTime where toValue = ZonedTime
instance ToValue Day       where toValue = Day
instance ToValue Integer   where toValue = Integer
instance ToValue Natural   where toValue = Integer . fromIntegral
instance ToValue Int       where toValue = Integer . fromIntegral
instance ToValue Int8      where toValue = Integer . fromIntegral
instance ToValue Int16     where toValue = Integer . fromIntegral
instance ToValue Int32     where toValue = Integer . fromIntegral
instance ToValue Int64     where toValue = Integer . fromIntegral
instance ToValue Word      where toValue = Integer . fromIntegral
instance ToValue Word8     where toValue = Integer . fromIntegral
instance ToValue Word16    where toValue = Integer . fromIntegral
instance ToValue Word32    where toValue = Integer . fromIntegral
instance ToValue Word64    where toValue = Integer . fromIntegral
