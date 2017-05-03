{-# OPTIONS_GHC -funbox-strict-fields #-}
{-|
Module      : Value
Description : /Internal:/ Value type for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Value where

import Data.Text (Text)
import Data.Time

-- | Values possible in a TOML file
data Value
  = TableV     [(Text,Value)] -- ^ table
  | ListV      [Value]        -- ^ array
  | DoubleV    !Double        -- ^ floating-point literal
  | IntegerV   !Integer       -- ^ integer literal
  | StringV    !Text          -- ^ string literal
  | BoolV      Bool           -- ^ boolean literal
  | ZonedTimeV !ZonedTime     -- ^ offset date-time
  | LocalTimeV !LocalTime     -- ^ local date-time
  | DayV       !Day           -- ^ local date
  | TimeOfDayV !TimeOfDay     -- ^ local time
  deriving (Read, Show)
