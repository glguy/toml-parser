{-|
Module      : Toml.ToValue
Description : Automation for converting application values to TOML.
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Toml.ToValue (
    ToValue(..),

    -- * Table construction
    table,
    (.=),
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map qualified as Map
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Toml.Value (Value(..))

table :: [(String, Value)] -> Value
table = Table . Map.fromList

(.=) :: ToValue a => String -> a -> (String, Value)
k .= v = (k, toValue v)

class ToValue a where
    toValue :: a -> Value

    toValueList :: [a] -> Value
    toValueList = Array . map toValue

instance ToValue Value where
    toValue v = v

-- | Single characters are encoded as singleton strings. Lists of characters
-- are encoded as a single string value.
instance ToValue Char where
    toValue x = String [x]
    toValueList = String

-- | This instance defers to the list element's 'toValueList' implementation.
instance ToValue a => ToValue [a] where
    toValue = toValueList

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
