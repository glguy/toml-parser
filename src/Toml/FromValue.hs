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

"Toml.FromValue.Generic" can be used to derive instances of 'FromTable'
automatically for record types.

-}
module Toml.FromValue (
    -- * Deserialization classes
    FromValue(..),
    FromTable(..),
    defaultTableFromValue,

    -- * Matcher
    Matcher,
    Result(..),
    runMatcher,
    withScope,
    inKey,
    inIndex,
    warning,

    -- * Table matching
    ParseTable,
    runParseTable,
    optKey,
    reqKey,
    warnTable,
    Alt(..),
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
import Toml.FromValue.Matcher (Matcher, Result(..), runMatcher, withScope, warning)
import Toml.Pretty (prettySimpleKey, prettyValue)
import Toml.Value (Value(..), Table)

-- | Class for types that can be decoded from a TOML value.
class FromValue a where
    -- | Convert a 'Value' or report an error message
    fromValue :: Value -> Matcher a

    -- | Used to implement instance for '[]'. Most implementations rely on the default implementation.
    listFromValue :: Value -> Matcher [a]
    listFromValue (Array xs) = zipWithM (\i v -> inIndex i (fromValue v)) [0..] xs
    listFromValue v = typeError "array" v

-- | Class for types that can be decoded from a TOML table.
class FromValue a => FromTable a where
    -- | Convert a 'Table' or report an error message
    fromTable :: Table -> Matcher a

instance (Ord k, IsString k, FromValue v) => FromTable (Map k v) where
    fromTable t = Map.fromList <$> traverse f (Map.assocs t)
        where
            f (k,v) = (,) (fromString k) <$> inKey k (fromValue v)

instance (Ord k, IsString k, FromValue v) => FromValue (Map k v) where
    fromValue = defaultTableFromValue

-- | Derive 'fromValue' implementation from 'fromTable'
defaultTableFromValue :: FromTable a => Value -> Matcher a
defaultTableFromValue (Table t) = fromTable t
defaultTableFromValue v = typeError "table" v

-- | Report a type error
typeError :: String {- ^ expected type -} -> Value {- ^ actual value -} -> Matcher a
typeError wanted got = fail ("type error. wanted: " ++ wanted ++ " got: " ++ valueType got)

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

-- | A 'Matcher' that tracks a current set of unmatched key-value
-- pairs from a table.
--
-- Use 'optKey' and 'reqKey' to extract keys.
--
-- Use 'getTable' and 'setTable' to override the table and implement
-- other primitives.
newtype ParseTable a = ParseTable (StateT Table Matcher a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadFail ParseTable where
    fail = ParseTable . fail

-- | Lift a matcher into the current table parsing context.
liftMatcher :: Matcher a -> ParseTable a
liftMatcher = ParseTable . lift

-- | Run a 'ParseTable' computation with a given starting 'Table'.
-- Unused tables will generate a warning. To change this behavior
-- 'getTable' and 'setTable' can be used to discard or generate
-- error messages.
runParseTable :: ParseTable a -> Table -> Matcher a
runParseTable (ParseTable p) t =
 do (x, t') <- runStateT p t
    case Map.keys t' of
        []  -> pure x
        [k] -> x <$ warning ("unexpected key: " ++ show (prettySimpleKey k))
        ks  -> x <$ warning ("unexpected keys: " ++ intercalate ", " (map (show . prettySimpleKey) ks))

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
optKey = optional . reqKey

-- | Match a table entry by key or report an error if missing.
reqKey :: FromValue a => String -> ParseTable a
reqKey key = pickKey [Key key fromValue]

-- | Key and value matching function
--
-- @since 1.1.2.0
data Alt a
    = Key String (Value -> Matcher a) -- ^ pick alternative based on key match
    | Else (Matcher a) -- ^ default case when no previous cases matched

-- | Take the first option from a list of table keys and matcher functions.
-- This operation will commit to the first table key that matches. If the
-- associated matcher fails, only that error will be propagated and the
-- other alternatives will not be matched.
--
-- If no keys match, an error message is generated explaining which keys
-- would have been accepted.
--
-- This is provided as an alternative to chaining multiple 'reqKey' cases
-- together with @('<|>')@ because that will generate one error message for
-- each unmatched alternative as well as the error associate with the
-- matched alternative.
--
-- @since 1.1.2.0
pickKey :: [Alt a] -> ParseTable a
pickKey xs =
 do t <- getTable
    foldr (f t) (fail errMsg) xs
    where
        f t (Else m) _ = liftMatcher m
        f t (Key k c) continue =
            case Map.lookup k t of
                Nothing -> continue
                Just v ->
                 do setTable $! Map.delete k t
                    liftMatcher (inKey k (c v))

        errMsg =
            case xs of
                []        -> "no alternatives"
                [Key k _] -> "missing key: " ++ show (prettySimpleKey k)
                _         -> "possible keys: " ++ intercalate ", " [show (prettySimpleKey k) | Key k _ <- xs]

-- | Update the scope with the message corresponding to a table key
--
-- @since 1.1.2.0
inKey :: String -> Matcher a -> Matcher a
inKey key = withScope ('.' : show (prettySimpleKey key))

-- | Update the scope with the message corresponding to an array index
--
-- @since 1.1.2.0
inIndex :: Int -> Matcher a -> Matcher a
inIndex i = withScope ("[" ++ show i ++ "]")