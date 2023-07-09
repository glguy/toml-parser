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

    -- * Matcher
    Matcher,
    Result(..),
    warning,

    -- * Table matching
    ParseTable,
    runParseTable,
    parseTableFromValue,
    optKey,
    reqKey,
    warnTable,
    KeyAlt(..),
    pickKey,

    -- * Table matching primitives
    getTable,
    setTable,
    liftMatcher,
    ) where

import Control.Applicative (Alternative, optional)
import Control.Monad (MonadPlus, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), put, get)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import Data.Time (ZonedTime, LocalTime, Day, TimeOfDay)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.FromValue.Matcher (Matcher, Result(..), runMatcher, withScope, warning, inIndex, inKey)
import Toml.Pretty (prettySimpleKey, prettyValue)
import Toml.Value (Value(..), Table)
import Toml.FromValue.ParseTable

-- | Class for types that can be decoded from a TOML value.
class FromValue a where
    -- | Convert a 'Value' or report an error message
    fromValue :: Value -> Matcher a

    -- | Used to implement instance for '[]'. Most implementations rely on the default implementation.
    listFromValue :: Value -> Matcher [a]
    listFromValue (Array xs) = zipWithM (\i v -> inIndex i (fromValue v)) [0..] xs
    listFromValue v = typeError "array" v

instance (Ord k, IsString k, FromValue v) => FromValue (Map k v) where
    fromValue (Table t) = Map.fromList <$> traverse f (Map.assocs t)
        where
            f (k,v) = (,) (fromString k) <$> inKey k (fromValue v)
    fromValue v = typeError "table" v

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

-- | Match a table entry by key if it exists or return 'Nothing' if not.
optKey :: FromValue a => String -> ParseTable (Maybe a)
optKey = optional . reqKey

-- | Match a table entry by key or report an error if missing.
reqKey :: FromValue a => String -> ParseTable a
reqKey key = pickKey [Key key fromValue]