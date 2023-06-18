module Raw where

import Data.List.NonEmpty (NonEmpty)
import Data.Time ( Day, LocalTime, TimeOfDay, ZonedTime )

type Key = NonEmpty String

data Expr
  = KeyValExpr     Int Key Val
  | TableExpr      Int Key
  | ArrayTableExpr Int Key
  deriving Show

data Val
  = ValInteger Integer
  | ValFloat Double
  | ValArray [Val]
  | ValTable [(Key, Val)]
  | ValBool Bool
  | ValString String
  | ValTimeOfDay TimeOfDay
  | ValZonedTime ZonedTime
  | ValLocalTime LocalTime
  | ValDay Day
  deriving Show
