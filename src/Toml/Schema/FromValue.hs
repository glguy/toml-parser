{-# Language TypeFamilies #-}
{-|
Module      : Toml.Schema.FromValue
Description : Automation for converting TOML values to application values.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'FromValue' to define a transformation from some 'Value' to an application
domain type.

Use 'ParseTable' to help build 'FromValue' instances that match tables. It
will make it easy to track which table keys have been used and which are left
over.

Warnings can be emitted using 'warn' and 'warnTable' (depending on what)
context you're in. These warnings can provide useful feedback about
problematic values or keys that might be unused now but were perhaps
meaningful in an old version of a configuration file.

"Toml.Schema.FromValue.Generic" can be used to derive instances of 'FromValue'
automatically for record types.

-}
module Toml.Schema.FromValue (
    -- * Deserialization classes
    FromValue(..),
    FromKey(..),

    -- * Containers
    mapOf,
    omapOf,
    listOf,

    -- * Tables
    parseTableFromValue,
    reqKey,
    reqKeyOf,
    optKey,
    optKeyOf,

    -- * Errors
    typeError,

    ) where

import Control.Monad (zipWithM, liftM2)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified
import Data.Time (ZonedTime, LocalTime, Day, TimeOfDay, UTCTime, zonedTimeToUTC)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.Schema.Matcher
import Toml.Schema.ParseTable
import Toml.Semantics

-- | Table matching function used to help implement 'fromValue' for tables.
-- Key matching function is given the annotation of the key for error reporting.
-- Value matching function is given the key in case values can depend on their keys.
mapOf ::
    Ord k =>
    (l -> Text -> Matcher l k)         {- ^ key matcher   -} ->
    (Text -> Value' l -> Matcher l v)  {- ^ value matcher -} ->
    Value' l -> Matcher l (Map k v)
mapOf matchKey matchVal = fmap Map.fromList . pairsOf matchKey matchVal

omapOf ::
    Ord k =>
    (l -> Text -> Matcher l k)         {- ^ key matcher   -} ->
    (Text -> Value' l -> Matcher l v)  {- ^ value matcher -} ->
    Value' l -> Matcher l (OMap k v)
omapOf matchKey matchVal = fmap OMap.fromList . pairsOf matchKey matchVal

-- | Shared helper to extract key/value pairs from a table using
-- a key matcher and a value matcher.
pairsOf ::
    Ord k =>
    (l -> Text -> Matcher l k)         {- ^ key matcher   -} ->
    (Text -> Value' l -> Matcher l v)  {- ^ value matcher -} ->
    Value' l -> Matcher l [(k, v)]
pairsOf matchKey matchVal =
    \case
        Table' _ (MkTable t) -> sequence
            [ liftM2 (,) (matchKey l k) (inKey k (matchVal k v))
            | (k, (l, v)) <- OMap.assocs t
            ]
        v -> typeError "table" v

-- | List matching function used to help implemented 'fromValue' for arrays.
-- The element matching function is given the list index in case values can
-- depend on their index.
listOf ::
    (Int -> Value' l -> Matcher l a) ->
    Value' l -> Matcher l [a]
listOf matchElt =
    \case
        List' _ xs -> zipWithM (\i -> inIndex i . matchElt i) [0..] xs
        v -> typeError "array" v

-- | Class for types that can be decoded from a TOML value.
class FromValue a where
    -- | Convert a 'Value' or report an error message
    fromValue :: Value' l -> Matcher l a

    -- | Used to implement instance for @[]@. Most implementations rely on the default implementation.
    listFromValue :: Value' l -> Matcher l [a]
    listFromValue = listOf (const fromValue)

instance (Ord k, FromKey k, FromValue v) => FromValue (Map k v) where
    fromValue = mapOf fromKey (const fromValue)

instance (Ord k, FromKey k, FromValue v) => FromValue (OMap k v) where
    fromValue = omapOf fromKey (const fromValue)

instance FromValue Table where
    fromValue (Table' _ t) = pure (forgetTableAnns t)
    fromValue v = typeError "table" v

-- | Convert from a table key
class FromKey a where
    fromKey :: l -> Text -> Matcher l a

-- | Matches all strings
instance a ~ Char => FromKey [a] where
    fromKey _ = pure . Text.unpack

-- | Matches all strings
instance FromKey Text where
    fromKey _ = pure

-- | Matches all strings
instance FromKey Data.Text.Lazy.Text where
    fromKey _ = pure . Data.Text.Lazy.fromStrict

-- | Report a type error
typeError :: String {- ^ expected type -} -> Value' l {- ^ actual value -} -> Matcher l a
typeError wanted got = failAt (valueAnn got) ("expected " ++ wanted ++ " but got " ++ valueType got)

-- | Used to derive a 'fromValue' implementation from a 'ParseTable' matcher.
parseTableFromValue :: ParseTable l a -> Value' l -> Matcher l a
parseTableFromValue p (Table' l t) = parseTable p l t
parseTableFromValue _ v = typeError "table" v

-- | Matches integer values
instance FromValue Integer where
    fromValue (Integer' _ x) = pure x
    fromValue v = typeError "integer" v

-- | Matches non-negative integer values
instance FromValue Natural where
    fromValue v =
     do i <- fromValue v
        if 0 <= i then
            pure (fromInteger i)
        else
            failAt (valueAnn v) "integer out of range for Natural"

fromValueSized :: forall l a. (Bounded a, Integral a) => String -> Value' l -> Matcher l a
fromValueSized name v =
 do i <- fromValue v
    if fromIntegral (minBound :: a) <= i && i <= fromIntegral (maxBound :: a) then
        pure (fromInteger i)
    else
        failAt (valueAnn v) ("integer out of range for " ++ name)

instance FromValue Int    where fromValue = fromValueSized "Int"
instance FromValue Int8   where fromValue = fromValueSized "Int8"
instance FromValue Int16  where fromValue = fromValueSized "Int16"
instance FromValue Int32  where fromValue = fromValueSized "Int32"
instance FromValue Int64  where fromValue = fromValueSized "Int64"
instance FromValue Word   where fromValue = fromValueSized "Word"
instance FromValue Word8  where fromValue = fromValueSized "Word8"
instance FromValue Word16 where fromValue = fromValueSized "Word16"
instance FromValue Word32 where fromValue = fromValueSized "Word32"
instance FromValue Word64 where fromValue = fromValueSized "Word64"

-- | Matches single-character strings with 'fromValue' and arbitrary
-- strings with 'listFromValue' to support 'Prelude.String'
instance FromValue Char where
    fromValue (Text' l t) =
        case Text.uncons t of
            Just (c, t')
                | Text.null t' -> pure c
            _ -> failAt l "expected single character"
    fromValue v = typeError "string" v

    listFromValue (Text' _ t) = pure (Text.unpack t)
    listFromValue v = typeError "string" v

-- | Matches string literals
instance FromValue Text where
    fromValue (Text' _ t) = pure t
    fromValue v = typeError "string" v

-- | Matches string literals
instance FromValue Data.Text.Lazy.Text where
    fromValue v = Data.Text.Lazy.fromStrict <$> fromValue v

-- | Matches floating-point and integer values
instance FromValue Double where
    fromValue (Double' _ x) = pure x
    fromValue (Integer' _ x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches floating-point and integer values
instance FromValue Float where
    fromValue (Double' _ x) = pure (realToFrac x)
    fromValue (Integer' _ x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches floating-point and integer values.
--
-- TOML specifies @Floats should be implemented as IEEE 754 binary64 values.@
-- so note that the given 'Rational' will be converted from a double
-- representation and will often be an approximation rather than the exact
-- value.
instance Integral a => FromValue (Ratio a) where
    fromValue (Double' a x)
        | isNaN x || isInfinite x = failAt a "finite float required"
        | otherwise = pure (realToFrac x)
    fromValue (Integer' _ x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches non-empty arrays or reports an error.
instance FromValue a => FromValue (NonEmpty a) where
    fromValue v =
     do xs <- fromValue v
        case NonEmpty.nonEmpty xs of
            Nothing -> failAt (valueAnn v) "non-empty list required"
            Just ne -> pure ne

-- | Matches arrays
instance FromValue a => FromValue (Seq a) where
    fromValue v = Seq.fromList <$> fromValue v

-- | Matches @true@ and @false@
instance FromValue Bool where
    fromValue (Bool' _ x) = pure x
    fromValue v = typeError "boolean" v

-- | Implemented in terms of 'listFromValue'
instance FromValue a => FromValue [a] where
    fromValue = listFromValue

-- | Matches local date literals
instance FromValue Day where
    fromValue (Day' _ x) = pure x
    fromValue v = typeError "local date" v

-- | Matches local time literals
instance FromValue TimeOfDay where
    fromValue (TimeOfDay' _ x) = pure x
    fromValue v = typeError "local time" v

-- | Matches offset date-time literals
instance FromValue ZonedTime where
    fromValue (ZonedTime' _ x) = pure x
    fromValue v = typeError "offset date-time" v

-- | Matches offset date-time literals and converts to UTC
instance FromValue UTCTime where
    fromValue (ZonedTime' _ x) = pure (zonedTimeToUTC x)
    fromValue v = typeError "offset date-time" v

-- | Matches local date-time literals
instance FromValue LocalTime where
    fromValue (LocalTime' _ x) = pure x
    fromValue v = typeError "local date-time" v

-- | Matches all values, used for pass-through
instance FromValue Value where
    fromValue = pure . forgetValueAnns

-- | Convenience function for matching an optional key with a 'FromValue'
-- instance.
--
-- @optKey key = 'optKeyOf' key 'fromValue'@
optKey :: FromValue a => Text -> ParseTable l (Maybe a)
optKey key = optKeyOf key fromValue

-- | Convenience function for matching a required key with a 'FromValue'
-- instance.
--
-- @reqKey key = 'reqKeyOf' key 'fromValue'@
reqKey :: FromValue a => Text -> ParseTable l a
reqKey key = reqKeyOf key fromValue

-- | Match a table entry by key if it exists or return 'Nothing' if not.
-- If the key is defined, it is matched by the given function.
--
-- See 'pickKey' for more complex cases.
optKeyOf ::
    Text                      {- ^ key           -} ->
    (Value' l -> Matcher l a) {- ^ value matcher -} ->
    ParseTable l (Maybe a)
optKeyOf key k = pickKey [Key key (fmap Just . k), Else (pure Nothing)]

-- | Match a table entry by key or report an error if missing.
--
-- See 'pickKey' for more complex cases.
reqKeyOf ::
    Text                      {- ^ key           -} ->
    (Value' l -> Matcher l a) {- ^ value matcher -} ->
    ParseTable l a
reqKeyOf key k = pickKey [Key key k]
