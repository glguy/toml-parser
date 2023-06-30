{-|
Module      : Toml.FromValue
Description : Automation for converting TOML values to application values.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Use 'FromValue' to define a transformation from some 'Value' to an application
domain type.

Use 'FromTable' to define transformations specifically from 'Table'. These
instances are interesting because all top-level TOML values are tables,
so these are the types that work for top-level deserialization.

Use 'ParseTable' to help build 'FromTable' instances. It will make it easy to
track which table keys have been used and which are left over.

Warnings can be emitted using 'warning' and 'warnTable' (depending on what)
context you're in. These warnings can provide useful feedback about
problematic decodings or keys that might be unused now but were perhaps
meaningful in an old version of a configuration file.

-}
module Toml.FromValue (
    -- * deserialization classes
    FromValue(..),
    FromTable(..),
    defaultTableFromValue,

    -- * matcher
    Matcher,
    runMatcher,
    withScope,
    warning,

    -- * results
    Result(..),

    -- * table matching
    ParseTable,
    runParseTable,
    optKey,
    reqKey,
    warnTable,

    -- * table matching primitives
    getTable,
    setTable,
    ) where

import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT, put, get)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import Data.Time (ZonedTime, LocalTime, Day, TimeOfDay)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.FromValue.Matcher (Matcher, Result(..), runMatcher, withScope, warning)
import Toml.Pretty (prettySimpleKey, prettyValue)
import Toml.Value (Value(..), Table)


-- | Class for types that can be decoded from a TOML value.
class FromValue a where
    -- | Convert a 'Value' or report an error message
    fromValue :: Value -> Matcher a

    -- | Used to implement instance for '[]'. Most implementations rely on the default implementation.
    listFromValue :: Value -> Matcher [a]
    listFromValue (Array xs) = zipWithM (\i v -> withScope ("[" ++ show i ++ "]") (fromValue v)) [0::Int ..] xs
    listFromValue v = typeError "array" v

-- | Class for types that can be decoded from a TOML table.
class FromValue a => FromTable a where
    -- | Convert a 'Table' or report an error message
    fromTable :: Table -> Matcher a

instance (Ord k, IsString k, FromValue v) => FromTable (Map k v) where
    fromTable t = Map.fromList <$> traverse f (Map.assocs t)
        where
            f (k,v) = (,) (fromString k) <$> withScope ('.':show (prettySimpleKey k)) (fromValue v)

instance (Ord k, IsString k, FromValue v) => FromValue (Map k v) where
    fromValue = defaultTableFromValue

-- | Derive 'fromValue' implementation from 'fromTable'
defaultTableFromValue :: FromTable a => Value -> Matcher a
defaultTableFromValue (Table t) = fromTable t
defaultTableFromValue v = typeError "table" v

-- | Report a type error
typeError :: String {- ^ expected type -} -> Value {- ^ actual value -} -> Matcher a
typeError wanted got = fail ("Type error. wanted: " ++ wanted ++ " got: " ++ show (prettyValue got))

instance FromValue Integer where
    fromValue (Integer x) = pure x
    fromValue v = typeError "integer" v

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

instance FromValue Char where
    fromValue (String [c]) = pure c
    fromValue v = typeError "character" v

    listFromValue (String xs) = pure xs
    listFromValue v = typeError "string" v

instance FromValue Double where
    fromValue (Float x) = pure x
    fromValue (Integer x) = pure (fromInteger x)
    fromValue v = typeError "float" v

instance FromValue Float where
    fromValue (Float x) = pure (realToFrac x)
    fromValue (Integer x) = pure (fromInteger x)
    fromValue v = typeError "float" v

instance FromValue Bool where
    fromValue (Bool x) = pure x
    fromValue v = typeError "boolean" v

instance FromValue a => FromValue [a] where
    fromValue = listFromValue

instance FromValue Day where
    fromValue (Day x) = pure x
    fromValue v = typeError "local date" v

instance FromValue TimeOfDay where
    fromValue (TimeOfDay x) = pure x
    fromValue v = typeError "local time" v

instance FromValue ZonedTime where
    fromValue (ZonedTime x) = pure x
    fromValue v = typeError "offset date-time" v

instance FromValue LocalTime where
    fromValue (LocalTime x) = pure x
    fromValue v = typeError "local date-time" v

instance FromValue Value where
    fromValue = pure

-- | A 'Matcher' that tracks a current set of unmatched key-value
-- pairs from a table.
--
-- Use 'optKey', 'reqKey', 'rej
newtype ParseTable a = ParseTable (StateT Table Matcher a)
    deriving (Functor, Applicative, Monad)

instance MonadFail ParseTable where
    fail = ParseTable . fail

-- | Run a 'ParseTable' computation with a given starting 'Table'.
-- Unused tables will generate a warning. To change this behavior
-- 'getTable' and 'setTable' can be used to discard or generate
-- error messages.
runParseTable :: ParseTable a -> Table -> Matcher a
runParseTable (ParseTable p) t =
 do (x, t') <- runStateT p t
    case Map.keys t' of
        []  -> pure x
        [k] -> x <$ warning ("Unexpected key: " ++ show (prettySimpleKey k))
        ks  -> x <$ warning ("Unexpected keys: " ++ intercalate ", " (map (show . prettySimpleKey) ks))

-- | Return the remaining portion of the table being matched.
getTable :: ParseTable Table
getTable = ParseTable get

-- | Replace the remaining portion of the table being matched.
setTable :: Table -> ParseTable ()
setTable = ParseTable . put

-- | Emit a warning at the current location.
warnTable :: String -> ParseTable ()
warnTable = ParseTable . lift . warning

-- | Match a table entry by key if it exists or return 'Nothing' if not.
optKey :: FromValue a => String -> ParseTable (Maybe a)
optKey key = ParseTable $ StateT \t ->
    case Map.lookup key t of
        Nothing -> pure (Nothing, t)
        Just v ->
         do r <- withScope ('.' : show (prettySimpleKey key)) (fromValue v)
            pure (Just r, Map.delete key t)

-- | Match a table entry by key or report an error if missing.
reqKey :: FromValue a => String -> ParseTable a
reqKey key =
 do mb <- optKey key
    case mb of
        Nothing -> fail ("Missing key: " ++ show (prettySimpleKey key))
        Just v -> pure v
