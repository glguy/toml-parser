{-|
Module      : Toml.Syntax.Token
Description : Lexical tokens
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the datatype for the lexical syntax of TOML files.
These tokens are generated by "Toml.Syntax.Lexer" and consumed in "Toml.Syntax.Parser".

-}
module Toml.Syntax.Token (
    -- * Types
    Token(..),

    -- * Integer literals
    mkBinInteger,
    mkDecInteger,
    mkOctInteger,
    mkHexInteger,

    -- * Float literals
    mkFloat,

    -- * Date and time patterns
    localDatePatterns,
    localTimePatterns,
    localDateTimePatterns,
    offsetDateTimePatterns,
    ) where

import Data.Char (digitToInt)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Text (Text)
import Numeric (readInt, readHex, readOct)

-- | Lexical token
data Token
    = TokTrue                       -- ^ @true@
    | TokFalse                      -- ^ @false@
    | TokComma                      -- ^ @','@
    | TokEquals                     -- ^ @'='@
    | TokNewline                    -- ^ @end-of-line@
    | TokPeriod                     -- ^ @'.'@
    | TokSquareO                    -- ^ @'['@
    | TokSquareC                    -- ^ @']'@
    | Tok2SquareO                   -- ^ @'[['@
    | Tok2SquareC                   -- ^ @']]'@
    | TokCurlyO                     -- ^ @'{'@
    | TokCurlyC                     -- ^ @'}'@
    | TokBareKey Text               -- ^ bare key
    | TokString Text                -- ^ string literal
    | TokMlString Text              -- ^ multiline string literal
    | TokInteger !Integer           -- ^ integer literal
    | TokFloat !Double              -- ^ floating-point literal
    | TokOffsetDateTime !ZonedTime  -- ^ date-time with timezone offset
    | TokLocalDateTime !LocalTime   -- ^ local date-time
    | TokLocalDate !Day             -- ^ local date
    | TokLocalTime !TimeOfDay       -- ^ local time
    | TokEOF                        -- ^ @end-of-input@
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

-- This wasn't added to base until 4.16
readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt

isBinDigit :: Char -> Bool
isBinDigit x = x == '0' || x == '1'

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