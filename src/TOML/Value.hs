{-# OPTIONS_GHC -funbox-strict-fields #-}
{-|
Module      : TOML.Value
Description : /Internal:/ Value type for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}

module TOML.Value where

import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)

-- | Values possible in a TOML file
data Value
  = Table      [(Text,Value)] -- ^ table, key-value pairs
  | List       [Value]        -- ^ array
  | Double     !Double        -- ^ floating-point literal
  | Integer    !Integer       -- ^ integer literal
  | String     !Text          -- ^ string literal
  | Bool       Bool           -- ^ boolean literal
  | ZonedTimeV !ZonedTime     -- ^ offset date-time
  | LocalTimeV !LocalTime     -- ^ local date-time
  | DayV       !Day           -- ^ local date
  | TimeOfDayV !TimeOfDay     -- ^ local time
  deriving (Read, Show)
