module Token where

import Data.Char (chr, isSpace)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Numeric (readBin, readHex, readOct)

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
  | TokInteger !Integer
  | TokFloat !Double
  | TokComment String
  | TokOffsetDateTime !ZonedTime
  | TokLocalDateTime !LocalTime
  | TokLocalDate !Day
  | TokLocalTime !TimeOfDay
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

mkDecInteger :: String -> Token
mkDecInteger ('+':xs) = TokInteger (read xs)
mkDecInteger xs = TokInteger (read xs)

mkHexInteger :: String -> Token
mkHexInteger ('0':'x':xs) = TokInteger (fst (head (readHex xs)))
mkHexInteger _ = error "processHex: bad input"

mkOctInteger :: String -> Token
mkOctInteger ('0':'o':xs) = TokInteger (fst (head (readOct xs)))
mkOctInteger _ = error "processHex: bad input"

mkBinInteger :: String -> Token
mkBinInteger ('0':'b':xs) = TokInteger (fst (head (readBin xs)))
mkBinInteger _ = error "processHex: bad input"

mkFloat :: String -> Token
mkFloat "nan"   = TokFloat (0/0)
mkFloat "+nan"  = TokFloat (0/0)
mkFloat "-nan"  = TokFloat (0/0)
mkFloat "inf"   = TokFloat (1/0)
mkFloat "+inf"  = TokFloat (1/0)
mkFloat "-inf"  = TokFloat (-1/0)
mkFloat ('+':x) = TokFloat (read x)
mkFloat x       = TokFloat (read x)

mkLiteralString :: String -> Token
mkLiteralString = TokString . tail . init

mkBasicString :: String -> Token
mkBasicString "" = error "processBasic: missing initializer"
mkBasicString (_:start) = TokString (go start)
  where
    go [] = error "processBasic: missing terminator"
    go "\"" = ""
    go ('\\':'"':xs) = '"' : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    go ('\\':'b':xs) = '\b' : go xs
    go ('\\':'f':xs) = '\f' : go xs
    go ('\\':'n':xs) = '\n' : go xs
    go ('\\':'r':xs) = '\r' : go xs
    go ('\\':'t':xs) = '\t' : go xs
    go ('\\':'u':a:b:c:d:xs) = chr (fst (head (readHex [a,b,c,d]))) : go xs
    go ('\\':'U':a:b:c:d:e:f:g:h:xs) = chr (fst (head (readHex [a,b,c,d,e,f,g,h]))) : go xs
    go (x:xs) = x : go xs

mkMlBasicString :: String -> Token
mkMlBasicString str =
  TokMlString
  case str of
    '"':'"':'"':'\r':'\n':start -> go start
    '"':'"':'"':'\n':start -> go start
    '"':'"':'"':start -> go start
    _ -> error "processMlBasic: missing initializer"
  where
    go "\"\"\"" = ""
    go ('\\':'"':xs) = '"' : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    go ('\\':'b':xs) = '\b' : go xs
    go ('\\':'f':xs) = '\f' : go xs
    go ('\\':'n':xs) = '\n' : go xs
    go ('\\':'r':xs) = '\r' : go xs
    go ('\\':'t':xs) = '\t' : go xs
    go ('\\':'u':a:b:c:d:xs) = chr (fst (head (readHex [a,b,c,d]))) : go xs
    go ('\\':'U':a:b:c:d:e:f:g:h:xs) = chr (fst (head (readHex [a,b,c,d,e,f,g,h]))) : go xs
    go ('\\':'\r':xs) = go (dropWhile isSpace xs)
    go ('\\':'\n':xs) = go (dropWhile isSpace xs)
    go ('\\':' ':xs)  = go (dropWhile isSpace xs)
    go ('\\':'\t':xs) = go (dropWhile isSpace xs)
    go (x:xs) = x : go xs
    go [] = error "processMlBasic: missing terminator"

mkMlLiteralString :: String -> Token
mkMlLiteralString str =
  TokMlString
  case str of
    '\'':'\'':'\'':'\r':'\n':start -> go start
    '\'':'\'':'\'':'\n':start -> go start
    '\'':'\'':'\'':start -> go start
    _ -> error "processMlLiteral: mising initializer"
  where
    go "'''" = ""
    go (x:xs) = x : go xs
    go "" = error "processMlLiteral: missing terminator"

localDatePatterns :: [String]
localDatePatterns = ["%Y-%m-%d"]

localTimePatterns :: [String]
localTimePatterns = ["%H:%M:%S%Q"]

localDateTimePatterns :: [String]
localDateTimePatterns =
  ["%Y-%m-%dT%H:%M:%S%Q",
   "%Y-%m-%d %H:%M:%S%Q"]

offsetDateTimePatterns :: [String]
offsetDateTimePatterns =
  ["%Y-%m-%dT%H:%M:%S%Q%Ez","%Y-%m-%dT%H:%M:%S%QZ",
   "%Y-%m-%d %H:%M:%S%Q%Ez","%Y-%m-%d %H:%M:%S%QZ"]
