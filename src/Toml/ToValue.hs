{-# LANGUAGE TypeFamilies #-} -- needed for type equality on old GHC
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
conversion to a 'Table'.

"Toml.ToValue.Generic" can be used to derive instances of 'ToTable'
automatically for record types.

-}
module Toml.ToValue (
    ToValue(..),

    -- * Table construction
    ToTable(..),
    ToKey(..),
    defaultTableToValue,
    table,
    (.=),
    ) where

import Data.Foldable (toList)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.Value

-- | Build a 'Table' from a list of key-value pairs.
--
-- Use '.=' for a convenient way to build the pairs.
--
-- @since 1.3.0.0
table :: [(Text, Value)] -> Table
table kvs = MkTable (Map.fromList [(k, ((), v)) | (k, v) <- kvs])
{-# INLINE table #-}

-- | Convenience function for building key-value pairs while
-- constructing a 'Table'.
--
-- @'table' [a '.=' b, c '.=' d]@
(.=) :: ToValue a => Text -> a -> (Text, Value)
k .= v = (k, toValue v)

-- | Class for types that can be embedded into 'Value'
class ToValue a where

    -- | Embed a single thing into a TOML value.
    toValue :: a -> Value

    -- | Helper for converting a list of things into a value. This is typically
    -- left to be defined by its default implementation and exists to help define
    -- the encoding for TOML arrays.
    toValueList :: [a] -> Value
    toValueList = List . map toValue

-- | Class for things that can be embedded into a TOML table.
--
-- Implement this for things that always embed into a 'Table' and then
-- the 'ToValue' instance can be derived with 'defaultTableToValue'.
--
-- @
-- instance ToValue Example where
--     toValue = defaultTableToValue
--
-- -- Option 1: Manual instance
-- instance ToTable Example where
--     toTable x = 'table' ["field1" '.=' field1 x, "field2" '.=' field2 x]
--
-- -- Option 2: GHC.Generics derived instance using Toml.ToValue.Generic
-- instance ToTable Example where
--     toTable = genericToTable
-- @
class ToValue a => ToTable a where

    -- | Convert a single value into a table
    toTable :: a -> Table

-- | @since 1.0.1.0
instance (ToKey k, ToValue v) => ToTable (Map k v) where
    toTable m = table [(toKey k, toValue v) | (k,v) <- Map.assocs m]

-- | @since 1.0.1.0
instance (ToKey k, ToValue v) => ToValue (Map k v) where
    toValue = defaultTableToValue

instance ToTable (Table' a) where
    toTable = forgetTableAnns

instance ToValue (Table' a) where
    toValue = defaultTableToValue

-- | Convert to a table key. This class enables various string types to be
-- used as the keys of a 'Map' when converting into TOML tables.
--
-- @since 1.3.0.0
class ToKey a where
    toKey :: a -> Text

-- | toKey = id
--
-- @since 1.3.0.0
instance Char ~ a => ToKey [a] where
    toKey = Text.pack

-- | toKey = unpack
--
-- @since 1.3.0.0
instance ToKey Text.Text where
    toKey = id

-- | toKey = unpack
--
-- @since 1.3.0.0
instance ToKey Data.Text.Lazy.Text where
    toKey = Data.Text.Lazy.toStrict

-- | Convenience function for building 'ToValue' instances.
defaultTableToValue :: ToTable a => a -> Value
defaultTableToValue = Table . toTable

-- | Identity function
instance ToValue Value where
    toValue = id

-- | Single characters are encoded as singleton strings. Lists of characters
-- are encoded as a single string value.
instance ToValue Char where
    toValue x = Text (Text.singleton x)
    toValueList = Text . Text.pack

-- | Encodes as string literal
--
-- @since 1.2.1.0
instance ToValue Text.Text where
    toValue = Text

-- | Encodes as string literal
--
-- @since 1.2.1.0
instance ToValue Data.Text.Lazy.Text where
    toValue = Text . Data.Text.Lazy.toStrict

-- | This instance defers to the list element's 'toValueList' implementation.
instance ToValue a => ToValue [a] where
    toValue = toValueList

-- | Converts to list and encodes that to value
--
-- @since 1.3.0.0
instance ToValue a => ToValue (NonEmpty a) where
    toValue = toValue . NonEmpty.toList

-- | Converts to list and encodes that to value
--
-- @since 1.3.0.0
instance ToValue a => ToValue (Seq a) where
    toValue = toValue . toList

-- | Converts to a 'Double'. This can overflow to infinity.
--
-- @since 1.3.0.0
instance Integral a => ToValue (Ratio a) where
    toValue = Double . realToFrac

instance ToValue Double    where toValue = Double
instance ToValue Float     where toValue = Double . realToFrac
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
