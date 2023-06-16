module Token where

import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime )

data Token
  = TokTrue
  | TokFalse
  | TokComma
  | TokEquals
  | TokNewline
  | TokPeriod
  | TokSquareO
  | TokSquareC
  | Tok2SquareO
  | Tok2SquareC
  | TokCurlyO
  | TokCurlyC
  | TokBareKey String
  | TokString String
  | TokMlString String
  | TokInteger Integer
  | TokFloat Double
  | TokComment String
  | TokOffsetDateTime ZonedTime
  | TokLocalDateTime LocalTime
  | TokLocalDate Day
  | TokLocalTime TimeOfDay
  | TokError String
  | TokEOF
  deriving Show

prettyToken :: Token -> String
prettyToken t =
  case t of
    TokTrue -> "true literal"
    TokFalse -> "false literal"
    TokComma -> "','"
    TokEquals -> "'='"
    TokNewline -> "newline"
    TokPeriod -> "'.'"
    TokSquareO -> "'['"
    TokSquareC -> "']'"
    Tok2SquareO -> "'[['"
    Tok2SquareC -> "']]]"
    TokCurlyO -> "'{'"
    TokCurlyC -> "'}'"
    TokBareKey key -> "bare key: " ++ key
    TokString str -> "string: " ++ show str
    TokMlString str -> "multi-line string: " ++ show str
    TokInteger str -> "integer: " ++ show str
    TokFloat str -> "float: " ++ show str
    TokComment _ -> "comment"
    TokOffsetDateTime _ -> "offset date-time"
    TokLocalDateTime _ -> "local date-time"
    TokLocalDate _ -> "local date"
    TokLocalTime _ -> "local time"
    TokError e -> "lexical error: " ++ e
    TokEOF -> "end-of-input"
