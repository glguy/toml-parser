{-|
Module      : Toml.Lexer.Token
Description : Lexical tokens
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the datatype for the lexical
syntax of TOML files. These tokens will drive the
parser in the "Parser" module.

-}
module Toml.Lexer.Token (
    Token(..),
    
    mkBasicString,
    mkLiteralString,
    mkMlBasicString,
    mkMlLiteralString,
    
    -- * integer literals
    mkBinInteger,
    mkDecInteger,
    mkOctInteger,
    mkHexInteger,

    -- * float literals
    mkFloat,

    -- * date and time patterns
    localDatePatterns,
    localTimePatterns,
    localDateTimePatterns,
    offsetDateTimePatterns,

    -- * errors
    mkError,
    ) where

import Data.Char (chr, isSpace)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Numeric (readBin, readHex, readOct)

-- | Lexical token
data Token
    = TokTrue                       -- ^ @true@
    | TokFalse                      -- ^ @false@
    | TokComma                      -- ^ @','@
    | TokEquals                     -- ^ @'='@
    | TokNewline                    -- ^ @'\\n'@
    | TokPeriod                     -- ^ @'.'@
    | TokSquareO                    -- ^ @'['@
    | TokSquareC                    -- ^ @']'@
    | Tok2SquareO                   -- ^ @'[['@
    | Tok2SquareC                   -- ^ @']]'@
    | TokCurlyO                     -- ^ @'{'@
    | TokCurlyC                     -- ^ @'}'@
    | TokBareKey String             -- ^ bare key
    | TokString String              -- ^ string literal
    | TokMlString String            -- ^ multiline string literal
    | TokInteger !Integer           -- ^ integer literal
    | TokFloat !Double              -- ^ floating-point literal
    | TokOffsetDateTime !ZonedTime  -- ^ date-time with timezone offset
    | TokLocalDateTime !LocalTime   -- ^ local date-time
    | TokLocalDate !Day             -- ^ local date
    | TokLocalTime !TimeOfDay       -- ^ local time
    | TokError String               -- ^ lexical error
    | TokEOF                        -- ^ end of file
    deriving (Read, Show)

-- | Remove underscores from number literals
scrub :: String -> String
scrub = filter ('_' /=)

-- | Construct a 'TokInteger' from a decimal integer literal lexeme.
mkDecInteger :: String -> Token
mkDecInteger ('+':xs) = TokInteger (read (scrub xs))
mkDecInteger xs = TokInteger (read (scrub xs))

-- | Construct a 'TokInteger' from a hexadecimal integer literal lexeme.
mkHexInteger :: String -> Token
mkHexInteger ('0':'x':xs) = TokInteger (fst (head (readHex (scrub xs))))
mkHexInteger _ = error "processHex: bad input"

-- | Construct a 'TokInteger' from a octal integer literal lexeme.
mkOctInteger :: String -> Token
mkOctInteger ('0':'o':xs) = TokInteger (fst (head (readOct (scrub xs))))
mkOctInteger _ = error "processHex: bad input"

-- | Construct a 'TokInteger' from a binary integer literal lexeme.
mkBinInteger :: String -> Token
mkBinInteger ('0':'b':xs) = TokInteger (fst (head (readBin (scrub xs))))
mkBinInteger _ = error "processHex: bad input"

-- | Construct a 'TokFloat' from a floating-point literal lexeme.
mkFloat :: String -> Token
mkFloat "nan"   = TokFloat (0/0)
mkFloat "+nan"  = TokFloat (0/0)
mkFloat "-nan"  = TokFloat (0/0)
mkFloat "inf"   = TokFloat (1/0)
mkFloat "+inf"  = TokFloat (1/0)
mkFloat "-inf"  = TokFloat (-1/0)
mkFloat ('+':x) = TokFloat (read (scrub x))
mkFloat x       = TokFloat (read (scrub x))

-- | Construct a 'TokString' from a literal string lexeme.
mkLiteralString :: String -> Token
mkLiteralString = TokString . tail . init

-- | Construct a 'TokString' from a basic string lexeme.
mkBasicString :: String -> Token
mkBasicString "" = error "processBasic: missing initializer"
mkBasicString (_:start) = enforceScalar TokString (go start)
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

-- | Construct a 'TokMlString' from a basic multi-line string lexeme.
mkMlBasicString :: String -> Token
mkMlBasicString str =
    enforceScalar TokMlString
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

-- | Construct a 'TokMlString' from a literal multi-line string lexeme.
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

enforceScalar :: (String -> Token) -> String -> Token
enforceScalar f str
    | any isInvalid str = TokError "string literal controls non-scalar value"
    | otherwise = f str
    where
        isInvalid x = '\xd800' <= x && x < '\xe000'

-- | Make a 'TokError' from a lexical error message.
mkError :: String -> Token
mkError str = TokError ("Lexical error: " ++ show (head str))

-- | Format strings for local date lexemes.
localDatePatterns :: [String]
localDatePatterns = ["%Y-%m-%d"]

-- | Format strings for local time lexemes.
localTimePatterns :: [String]
localTimePatterns = ["%H:%M:%S%Q"]

-- | Format strings for local datetime lexemes.
localDateTimePatterns :: [String]
localDateTimePatterns =
    ["%Y-%m-%dT%H:%M:%S%Q",
    "%Y-%m-%d %H:%M:%S%Q"]

-- | Format strings for offset datetime lexemes.
offsetDateTimePatterns :: [String]
offsetDateTimePatterns =
    ["%Y-%m-%dT%H:%M:%S%Q%Ez","%Y-%m-%dT%H:%M:%S%QZ",
    "%Y-%m-%d %H:%M:%S%Q%Ez","%Y-%m-%d %H:%M:%S%QZ"]
