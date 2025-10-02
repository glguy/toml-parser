{-# Language PatternSynonyms, DeriveTraversable, TypeFamilies #-}
{-|
Module      : Toml.Semantics.Types
Description : Semantic TOML values
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the type for the semantics of a TOML file.
All dotted keys are resolved in this representation. Each table
is an ordered map with a single level of keys.

Values are parameterized over an annotation type to allow values
to be attributed to a file location. When values are constructed
programmatically, there might not be any interesting annotations.
In this case a trivial @()@ unit annotation can be used. The
'Value' type-synonym and related pattern synonyms can make using
this case more convenient.

-}
module Toml.Semantics.Types (
    -- * Unlocated value synonyms
    Value,
    Table,

    -- * Annotated values
    Value'(..,
        Integer, Double, Text, Bool,
        ZonedTime, Day, LocalTime, TimeOfDay,
        List, Table),
    Table'(..),

    -- * Utilities
    forgetValueAnns,
    forgetTableAnns,
    valueAnn,
    valueType,
    ) where

import Data.Map.Ordered (OMap)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime(zonedTimeToLocalTime, zonedTimeZone), timeZoneMinutes)

pattern Integer :: Integer -> Value
pattern Integer x <- Integer' _ x
    where Integer x = Integer' () x

pattern Double :: Double -> Value
pattern Double x <- Double' _ x
    where Double x = Double' () x

pattern List :: [Value] -> Value
pattern List x <- List' _ x
    where List x = List' () x

pattern Table :: Table -> Value
pattern Table x <- Table' _ x
    where Table x = Table' () x

pattern Bool :: Bool -> Value
pattern Bool x <- Bool' _ x
    where Bool x = Bool' () x

pattern Text :: Text -> Value
pattern Text x <- Text' _ x
    where Text x = Text' () x

pattern TimeOfDay :: TimeOfDay -> Value
pattern TimeOfDay x <- TimeOfDay' _ x
    where TimeOfDay x = TimeOfDay' () x

pattern ZonedTime :: ZonedTime -> Value
pattern ZonedTime x <- ZonedTime' _ x
    where ZonedTime x = ZonedTime' () x

pattern LocalTime :: LocalTime -> Value
pattern LocalTime x <- LocalTime' _ x
    where LocalTime x = LocalTime' () x

pattern Day :: Day -> Value
pattern Day x <- Day' _ x
    where Day x = Day' () x

{-# Complete List, Table, Text, Bool, Integer, Double, Day, LocalTime, ZonedTime, TimeOfDay #-}

-- | Semantic TOML value with all table assignments resolved.
data Value' a
    = Integer'   a Integer
    | Double'    a Double
    | List'      a [Value' a]
    | Table'     a (Table' a)
    | Bool'      a Bool
    | Text'      a Text
    | TimeOfDay' a TimeOfDay
    | ZonedTime' a ZonedTime
    | LocalTime' a LocalTime
    | Day'       a Day
    deriving (
        Show        {- ^ Default instance -},
        Read        {- ^ Default instance -},
        Functor     {- ^ Derived          -},
        Foldable    {- ^ Derived          -},
        Traversable {- ^ Derived          -})

-- | Extract the top-level annotation from a value.
valueAnn :: Value' a -> a
valueAnn = \case
    Integer'   a _ -> a
    Double'    a _ -> a
    List'      a _ -> a
    Table'     a _ -> a
    Bool'      a _ -> a
    Text'      a _ -> a
    TimeOfDay' a _ -> a
    ZonedTime' a _ -> a
    LocalTime' a _ -> a
    Day'       a _ -> a

-- | String representation of the kind of value using TOML vocabulary
valueType :: Value' l -> String
valueType = \case
    Integer'   {} -> "integer"
    Double'    {} -> "float"
    List'      {} -> "array"
    Table'     {} -> "table"
    Bool'      {} -> "boolean"
    Text'      {} -> "string"
    TimeOfDay' {} -> "local time"
    LocalTime' {} -> "local date-time"
    Day'       {} -> "locate date"
    ZonedTime' {} -> "offset date-time"

-- | A table with annotated keys and values.
newtype Table' a = MkTable (OMap Text (a, Value' a))
    deriving (
        Show        {- ^ Default instance -},
        Read        {- ^ Default instance -},
        Eq          {- ^ Default instance -},
        Functor     {- ^ Derived          -},
        Foldable    {- ^ Derived          -},
        Traversable {- ^ Derived          -})

-- | A 'Table'' with trivial annotations
type Table = Table' ()

-- | A 'Value'' with trivial annotations
type Value = Value' ()

-- | Replaces annotations with a unit.
forgetTableAnns :: Table' a -> Table
forgetTableAnns (MkTable t) = MkTable (fmap (\(_, v) -> ((), forgetValueAnns v)) t)

-- | Replaces annotations with a unit.
forgetValueAnns :: Value' a -> Value
forgetValueAnns =
    \case
        Integer'   _ x -> Integer   x
        Double'    _ x -> Double    x
        List'      _ x -> List      (map forgetValueAnns x)
        Table'     _ x -> Table     (forgetTableAnns x)
        Bool'      _ x -> Bool      x
        Text'      _ x -> Text      x
        TimeOfDay' _ x -> TimeOfDay x
        ZonedTime' _ x -> ZonedTime x
        LocalTime' _ x -> LocalTime x
        Day'       _ x -> Day       x

-- | Nearly default instance except 'ZonedTime' doesn't have an
-- 'Eq' instance. 'ZonedTime' values are equal if their times and
-- time-zones are both equal.
instance Eq a => Eq (Value' a) where
    Integer'   a x == Integer'   b y = a == b && x == y
    Double'    a x == Double'    b y = a == b && x == y
    List'      a x == List'      b y = a == b && x == y
    Table'     a x == Table'     b y = a == b && x == y
    Bool'      a x == Bool'      b y = a == b && x == y
    Text'      a x == Text'      b y = a == b && x == y
    TimeOfDay' a x == TimeOfDay' b y = a == b && x == y
    LocalTime' a x == LocalTime' b y = a == b && x == y
    Day'       a x == Day'       b y = a == b && x == y
    ZonedTime' a x == ZonedTime' b y = a == b && projectZT x == projectZT y
    _              == _              = False

-- Extract the relevant parts to build an 'Eq' instance
projectZT :: ZonedTime -> (LocalTime, Int)
projectZT x = (zonedTimeToLocalTime x, timeZoneMinutes (zonedTimeZone x))

-- | Constructs a TOML string literal.
--
-- @
-- fromString = String
-- @
instance () ~ a => IsString (Value' a) where
    fromString = Text . fromString
