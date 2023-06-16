module Raw where

import Data.Time ( Day, LocalTime, TimeOfDay, ZonedTime )

data Expr
  = KeyValExpr [String] Val
  | TableExpr [String]
  | ArrayTableExpr [String]
  deriving Show

data Val
  = ValInteger Integer
  | ValFloat Double
  | ValArray [Val]
  | ValTable [([String], Val)]
  | ValBool Bool
  | ValString String
  | ValTimeOfDay TimeOfDay
  | ValZonedTime ZonedTime
  | ValLocalTime LocalTime
  | ValDay Day
  deriving Show
