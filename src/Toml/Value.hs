{-|
Module      : Toml.Value
Description : Semantic TOML values
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the type for the semantics of a TOML file.
All dotted keys are resolved in this representation. Each table
is a Map with a single level of keys.

-}
module Toml.Value (
    Value(..),
    Table,
    ) where

import Data.Data (Data)
import Data.Map (Map)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime(zonedTimeToLocalTime, zonedTimeZone), timeZoneMinutes)
import GHC.Generics (Generic)

-- | Representation of a TOML key-value table.
type Table = Map String Value

-- | Semantic TOML value with all table assignments resolved.
data Value
    = Integer   Integer
    | Float     Double
    | Array     [Value]
    | Table     Table
    | Bool      Bool
    | String    String
    | TimeOfDay TimeOfDay
    | ZonedTime ZonedTime
    | LocalTime LocalTime
    | Day       Day
    deriving (
        Show {- ^ Default instance -}, 
        Read {- ^ Default instance -},
        Data {- ^ Default instance -},
        Generic {- ^ Default instance -})

-- | Nearly default instance except 'ZonedTime' doesn't have an
-- 'Eq' instance. 'ZonedTime' values are equal if their times and
-- timezones are both equal.

instance Eq Value where
    Integer   x == Integer   y = x == y
    Float     x == Float     y = x == y
    Array     x == Array     y = x == y
    Table     x == Table     y = x == y
    Bool      x == Bool      y = x == y
    String    x == String    y = x == y
    TimeOfDay x == TimeOfDay y = x == y
    LocalTime x == LocalTime y = x == y
    Day       x == Day       y = x == y
    ZonedTime x == ZonedTime y = projectZT x == projectZT y
    _           == _           = False

-- Extract the relevant parts to build an Eq instance
projectZT :: ZonedTime -> (LocalTime, Int)
projectZT x = (zonedTimeToLocalTime x, timeZoneMinutes (zonedTimeZone x))
