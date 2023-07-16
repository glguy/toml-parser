{-# Language TypeFamilies #-}
{-|
Module      : Toml.FromValue
Description : Automation for converting TOML values to application values.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'FromValue' to define a transformation from some 'Value' to an application
domain type.

Use 'ParseTable' to help build 'FromValue' instances that match tables. It
will make it easy to track which table keys have been used and which are left
over.

Warnings can be emitted using 'warning' and 'warnTable' (depending on what)
context you're in. These warnings can provide useful feedback about
problematic decodings or keys that might be unused now but were perhaps
meaningful in an old version of a configuration file.

"Toml.FromValue.Generic" can be used to derive instances of 'FromValue'
automatically for record types.

-}
module Toml.FromValue (
    -- * Deserialization classes
    FromValue(..),
    FromKey(..),

    -- * Matcher
    Matcher,
    MatchMessage(..),
    Result(..),
    warning,

    -- * Table matching
    ParseTable,
    runParseTable,
    parseTableFromValue,
    reqKey,
    optKey,
    reqKeyOf,
    optKeyOf,
    warnTable,
    KeyAlt(..),
    pickKey,

    -- * Table matching primitives
    getTable,
    setTable,
    liftMatcher,
    ) where

import Control.Monad (zipWithM)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Time (ZonedTime, LocalTime, Day, TimeOfDay)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.FromValue.Matcher (Matcher, Result(..), MatchMessage(..), warning, inIndex, inKey)
import Toml.FromValue.ParseTable
import Toml.Value (Value(..))

-- | Class for types that can be decoded from a TOML value.
class FromValue a where
    -- | Convert a 'Value' or report an error message
    fromValue :: Value -> Matcher a

    -- | Used to implement instance for '[]'. Most implementations rely on the default implementation.
    listFromValue :: Value -> Matcher [a]
    listFromValue (Array xs) = zipWithM (\i v -> inIndex i (fromValue v)) [0..] xs
    listFromValue v = typeError "array" v

instance (Ord k, FromKey k, FromValue v) => FromValue (Map k v) where
    fromValue (Table t) = Map.fromList <$> traverse f (Map.assocs t)
        where
            f (k,v) = (,) <$> fromKey k <*> inKey k (fromValue v)
    fromValue v = typeError "table" v

-- | Convert from a table key
--
-- @since 1.3.0.0
class FromKey a where
    fromKey :: String -> Matcher a

-- | Matches all strings
--
-- @since 1.3.0.0
instance a ~ Char => FromKey [a] where
    fromKey = pure

-- | Matches all strings
--
-- @since 1.3.0.0
instance FromKey Data.Text.Text where
    fromKey = pure . Data.Text.pack

-- | Matches all strings
--
-- @since 1.3.0.0
instance FromKey Data.Text.Lazy.Text where
    fromKey = pure . Data.Text.Lazy.pack

-- | Report a type error
typeError :: String {- ^ expected type -} -> Value {- ^ actual value -} -> Matcher a
typeError wanted got = fail ("type error. wanted: " ++ wanted ++ " got: " ++ valueType got)

-- | Used to derive a 'fromValue' implementation from a 'ParseTable' matcher.
parseTableFromValue :: ParseTable a -> Value -> Matcher a
parseTableFromValue p (Table t) = runParseTable p t
parseTableFromValue _ v = typeError "table" v

valueType :: Value -> String
valueType = \case
    Integer   {} -> "integer"
    Float     {} -> "float"
    Array     {} -> "array"
    Table     {} -> "table"
    Bool      {} -> "boolean"
    String    {} -> "string"
    TimeOfDay {} -> "local time"
    LocalTime {} -> "local date-time"
    Day       {} -> "locate date"
    ZonedTime {} -> "offset date-time"

-- | Matches integer values
instance FromValue Integer where
    fromValue (Integer x) = pure x
    fromValue v = typeError "integer" v

-- | Matches non-negative integer values
instance FromValue Natural where
    fromValue v =
     do i <- fromValue v
        if 0 <= i then
            pure (fromInteger i)
        else
            fail "integer out of range for Natural"

fromValueSized :: forall a. (Bounded a, Integral a) => String -> Value -> Matcher a
fromValueSized name v =
 do i <- fromValue v
    if fromIntegral (minBound :: a) <= i && i <= fromIntegral (maxBound :: a) then
        pure (fromInteger i)
    else
        fail ("integer out of range for " ++ name)

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
    fromValue (String [c]) = pure c
    fromValue v = typeError "character" v

    listFromValue (String xs) = pure xs
    listFromValue v = typeError "string" v

-- | Matches string literals
--
-- @since 1.2.1.0
instance FromValue Data.Text.Text where
    fromValue v = Data.Text.pack <$> fromValue v

-- | Matches string literals
--
-- @since 1.2.1.0
instance FromValue Data.Text.Lazy.Text where
    fromValue v = Data.Text.Lazy.pack <$> fromValue v

-- | Matches floating-point and integer values
instance FromValue Double where
    fromValue (Float x) = pure x
    fromValue (Integer x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches floating-point and integer values
instance FromValue Float where
    fromValue (Float x) = pure (realToFrac x)
    fromValue (Integer x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches floating-point and integer values.
--
-- TOML specifies @Floats should be implemented as IEEE 754 binary64 values.@
-- so note that the given 'Rational' will be converted from a double
-- representation and will often be an approximation rather than the exact
-- value.
--
-- @since 1.3.0.0
instance Integral a => FromValue (Ratio a) where
    fromValue (Float x)
        | isNaN x || isInfinite x = fail "finite float required"
        | otherwise = pure (realToFrac x)
    fromValue (Integer x) = pure (fromInteger x)
    fromValue v = typeError "float" v

-- | Matches non-empty arrays or reports an error.
--
-- @since 1.3.0.0
instance FromValue a => FromValue (NonEmpty a) where
    fromValue v =
     do xs <- fromValue v
        case NonEmpty.nonEmpty xs of
            Nothing -> fail "non-empty list required"
            Just ne -> pure ne

-- | Matches arrays
--
-- @since 1.3.0.0
instance FromValue a => FromValue (Seq a) where
    fromValue v = Seq.fromList <$> fromValue v

-- | Matches @true@ and @false@
instance FromValue Bool where
    fromValue (Bool x) = pure x
    fromValue v = typeError "boolean" v

-- | Implemented in terms of 'listFromValue'
instance FromValue a => FromValue [a] where
    fromValue = listFromValue

-- | Matches local date literals
instance FromValue Day where
    fromValue (Day x) = pure x
    fromValue v = typeError "local date" v

-- | Matches local time literals
instance FromValue TimeOfDay where
    fromValue (TimeOfDay x) = pure x
    fromValue v = typeError "local time" v

-- | Matches offset date-time literals
instance FromValue ZonedTime where
    fromValue (ZonedTime x) = pure x
    fromValue v = typeError "offset date-time" v

-- | Matches local date-time literals
instance FromValue LocalTime where
    fromValue (LocalTime x) = pure x
    fromValue v = typeError "local date-time" v

-- | Matches all values, used for pass-through
instance FromValue Value where
    fromValue = pure

-- | Convenience function for matching an optional key with a 'FromValue'
-- instance.
--
-- @optKey key = 'optKeyOf' key 'fromValue'@
optKey :: FromValue a => String -> ParseTable (Maybe a)
optKey key = optKeyOf key fromValue

-- | Convenience function for matching a required key with a 'FromValue'
-- instance.
--
-- @reqKey key = 'reqKeyOf' key 'fromValue'@
reqKey :: FromValue a => String -> ParseTable a
reqKey key = reqKeyOf key fromValue

-- | Match a table entry by key if it exists or return 'Nothing' if not.
-- If the key is defined, it is matched by the given function.
--
-- See 'pickKey' for more complex cases.
optKeyOf ::
    String {- ^ key -} ->
    (Value -> Matcher a) {- ^ value matcher -} ->
    ParseTable (Maybe a)
optKeyOf key k = pickKey [Key key (fmap Just . k), Else (pure Nothing)]

-- | Match a table entry by key or report an error if missing.
--
-- See 'pickKey' for more complex cases.
reqKeyOf ::
    String {- ^ key -} ->
    (Value -> Matcher a) {- ^ value matcher -} ->
    ParseTable a
reqKeyOf key k = pickKey [Key key k]
