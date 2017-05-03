module Value where

import Data.Text (Text)
import Data.Time

data Value
  = TableV [(Text,Value)]
  | ListV  [Value]
  | DoubleV Double
  | IntegerV Integer
  | StringV Text
  | BoolV Bool
  | ZonedTimeV ZonedTime
  | LocalTimeV LocalTime
  | DayV Day
  | TimeOfDayV TimeOfDay
  deriving (Read, Show)
