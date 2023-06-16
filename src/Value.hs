module Value where

import Data.Map (Map)
import Data.Time ( Day, LocalTime, TimeOfDay, ZonedTime )

data Value
  = Integer Integer
  | Float Double
  | Array [Value]
  | Table (Map String Value)
  | Bool Bool
  | String String
  | TimeOfDay TimeOfDay
  | ZonedTime ZonedTime
  | LocalTime LocalTime
  | Day Day
  deriving (Show, Read)
