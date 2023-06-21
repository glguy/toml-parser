{-|
Module      : Toml.FromValue
Description : Automation for converting TOML values to application values.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Toml.FromValue (
    FromValue(..),
    FromTable(..),

    -- * table matching
    ParseTable,
    defaultTableFromValue,
    runParseTable,
    optKey,
    reqKey,
    discardKeys,
    ) where

import Toml.Value (Value(..), Table)
import Toml.Pretty (prettySimpleKey, prettyValue)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Int ( Int8, Int16, Int32, Int64 )
import Numeric.Natural (Natural)
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Time (ZonedTime, LocalTime, Day, TimeOfDay)
import Control.Monad.Trans.State (StateT(..), put)
import Data.List (intercalate)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)

class FromValue a where
    fromValue     :: Value -> Either String a
    listFromValue :: Value -> Either String [a]
    listFromValue (Array xs) = zipWithM (\i v -> fromValue v `backtrace` ("[" ++ show i ++ "]")) [0::Int ..] xs

class FromValue a => FromTable a where
    fromTable :: Table -> Either String a

defaultTableFromValue (Table t) = fromTable t
defaultTableFromValue v = typeError "table" v

typeError :: String -> Value -> Either String a
typeError wanted got = Left ("Type error. wanted: " ++ wanted ++ " got: " ++ prettyValue got)

instance FromValue Integer where
    fromValue (Integer x) = Right x
    fromValue v = typeError "integer" v

instance FromValue Natural where
    fromValue (Integer x)
        | 0 <= x = Right (fromInteger x)
        | otherwise = Left "integer out of range for Natural"
    fromValue v = typeError "integer" v

fromValueSized :: forall a. (Bounded a, Integral a) => String -> Value -> Either String a
fromValueSized name (Integer x)
    | fromIntegral (minBound :: a) <= x, x <= fromIntegral (maxBound :: a) = Right (fromInteger x)
    | otherwise = Left ("integer out of range for " ++ name)
fromValueSized _ v = typeError "integer" v

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
    fromValue (String [c]) = Right c
    fromValue v = typeError "character" v

    listFromValue (String xs) = Right xs
    listFromValue v = typeError "string" v

instance FromValue Double where
    fromValue (Float x) = Right x
    fromValue (Integer x) = Right (fromInteger x)
    fromValue v = typeError "float" v

instance FromValue Float where
    fromValue (Float x) = Right (realToFrac x)
    fromValue (Integer x) = Right (fromInteger x)
    fromValue v = typeError "float" v

instance FromValue Bool where
    fromValue (Bool x) = Right x
    fromValue v = typeError "boolean" v

instance FromValue a => FromValue [a] where
    fromValue = listFromValue

instance FromValue Day where
    fromValue (Day x) = Right x
    fromValue v = typeError "local date" v

instance FromValue TimeOfDay where
    fromValue (TimeOfDay x) = Right x
    fromValue v = typeError "local time" v

instance FromValue ZonedTime where
    fromValue (ZonedTime x) = Right x
    fromValue v = typeError "offset date-time" v

instance FromValue LocalTime where
    fromValue (LocalTime x) = Right x
    fromValue v = typeError "local date-time" v

instance FromValue Value where
    fromValue = Right

newtype ParseTable a = ParseTable (StateT Table (Either String) a)
    deriving (Functor, Applicative, Monad)

instance MonadFail ParseTable where
    fail = ParseTable . lift . Left

backtrace :: Either String a -> String -> Either String a
Right x `backtrace` _ = Right x
Left  e `backtrace` l = Left (e ++ " in " ++ l)

runParseTable :: ParseTable a -> Table -> Either String a
runParseTable (ParseTable p) t =
 do (r, t') <- runStateT p t
    if Map.null t' then
        Right r
    else
        Left ("Unmatched keys: " ++ intercalate ", " (map prettySimpleKey (Map.keys t')))

optKey :: FromValue a => String -> ParseTable (Maybe a)
optKey key = ParseTable $ StateT \t ->
    case Map.lookup key t of
        Nothing -> Right (Nothing, t)
        Just v ->
         do r <- fromValue v `backtrace` ('.' : prettySimpleKey key)
            pure (Just r, Map.delete key t)

reqKey :: FromValue a => String -> ParseTable a
reqKey key =
 do mb <- optKey key
    case mb of
        Nothing -> fail ("Missing key: " ++ prettyValue (String key))
        Just v -> pure v

-- | Discard the remainder of the table to ignore any unused keys
discardKeys :: ParseTable ()
discardKeys = ParseTable (put Map.empty)
