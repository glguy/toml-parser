{-# LANGUAGE BangPatterns #-}
{-|
Module      : TOML.LexerUtils
Description : /Internal:/ Lexer support operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module is separate from the Lexer.x input to Alex
to segregate the automatically generated code from the
hand written code. The automatically generated code
causes lots of warnings which mask the interesting warnings.
-}
module TOML.LexerUtils
  (
  -- * Alex required definitions
    AlexInput
  , alexGetByte

  -- * Lexer modes
  , LexerMode(..)
  , lexerModeInt

  -- * Lexer actions
  , Action
  , token
  , token_
  , errorAction
  , eofAction

  -- * Token parsers
  , integer
  , double
  , bareKeyToken

  -- * String literal actions
  , startString
  , emitChar
  , emitChar'
  , emitUnicodeChar
  , endString

  -- * Date/time token parsers
  , localtime
  , zonedtime
  , day
  , timeofday
  ) where

import           Data.Char (isSpace, isControl, isAscii, ord, chr)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Data.Time (ParseTime, parseTimeOrError, defaultTimeLocale, iso8601DateFormat)
import           Data.Word (Word8)

import           TOML.Tokens
import           TOML.Located

------------------------------------------------------------------------
-- Custom Alex wrapper - these functions are used by generated code
------------------------------------------------------------------------

-- | The generated code expects the lexer input type to be named 'AlexInput'
type AlexInput = Located Text

-- | Get the next characteristic byte from the input source.
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (c,cs') <- Text.uncons cs
       let !b   = byteForChar c
           !inp = Located (move p c) cs'
       return (b, inp)

-- | The TOML format doesn't distinguish between any of the non-ASCII
-- characters. This function extracts the printable and whitespace
-- subset of Unicode and maps it to the ASCII value as used by Alex.
byteForChar :: Char -> Word8
byteForChar c
  | isControl c && not (isSpace c) = 0
  | isAscii c = fromIntegral (ord c)
  | otherwise = 0

------------------------------------------------------------------------

-- | Advance the position according to the kind of character lexed.
move :: Position -> Char -> Position
move (Position ix line column) c =
  case c of
    '\t' -> Position (ix + 1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> Position (ix + 1) (line + 1) 1
    _    -> Position (ix + 1) line (column + 1)

------------------------------------------------------------------------
-- Lexer Modes
------------------------------------------------------------------------

-- | The lexer can be in a normal mode or can be lexing a string literal.
data LexerMode
  = InNormal
  | InString !Int !Position String
    -- ^ alex-mode, starting-position, reversed accumulated characters
  deriving Show


-- | Compute the Alex state corresponding to a particular 'LexerMode'
lexerModeInt :: LexerMode -> Int
lexerModeInt InNormal{}           = 0
lexerModeInt (InString mode _ _)  = mode


------------------------------------------------------------------------
-- Lexer actions
------------------------------------------------------------------------

-- | Type of actions used by lexer upon matching a rule
type Action =
  Located Text                 {- ^ located lexeme                     -} ->
  LexerMode                    {- ^ lexer mode                         -} ->
  (LexerMode, [Located Token]) {- ^ updated lexer mode, emitted tokens -}


-- | Helper function for building an 'Action' using the lexeme
token :: (Text -> Token) {- ^ lexeme -> token -} -> Action
token f match st = (st, [fmap f match])

-- | Helper function for building an 'Action' where the lexeme is unused.
token_ :: Token -> Action
token_ = token . const

-- | Action to perform upon end of file. Produce errors if EOF was unexpected.
eofAction :: Position -> LexerMode -> [Located Token]
eofAction eofPosn st =
  case st of
    InString _ posn _ -> [Located posn (ErrorToken UntermString)]
    InNormal          -> [Located eofPosn EofToken]

-- | Action to perform when lexer gets stuck. Emits an error.
errorAction :: AlexInput -> [Located Token]
errorAction inp = [fmap (ErrorToken . NoMatch . Text.head) inp]

------------------------------------------------------------------------
-- String literal mode actions
------------------------------------------------------------------------

-- | Enter the string literal lexer
startString :: Int -> Action
startString mode lexeme _ = (InString mode (locPosition lexeme) [], [])


-- | Add current lexeme to the current string literal.
emitChar :: Action
emitChar _ InNormal = error "PANIC: emitChar used in normal mode"
emitChar lexeme (InString mode pos acc) = (InString mode pos acc', [])
  where
    acc' = reverse (Text.unpack (locThing lexeme)) ++ acc


-- | Add literal character to the current string literal.
emitChar' :: Char -> Action
emitChar' c _ (InString mode pos acc) = (InString mode pos (c : acc), [])
emitChar' _ _ _ = error "PANIC: emitChar' used in normal mode"


-- | Interpret the current lexeme as a unicode escape sequence and add
-- the resulting character to the current string literal.
emitUnicodeChar :: Action
emitUnicodeChar lexeme mode =
  case Text.hexadecimal (Text.drop 2 (locThing lexeme)) of
    Right (n, _)
      | n < 0x110000 -> emitChar' (chr n) lexeme mode
      | otherwise    -> (InNormal, [Located (locPosition lexeme) (ErrorToken BadEscape)])
    _ -> error "PANIC: bad unicode unescape implementation"


-- | Successfully terminate the current mode and emit tokens as needed
endString :: Action
endString _ mode =
  case mode of
    InNormal -> error "PANIC: error in string literal lexer"
    InString _ p input ->
      let !str = Text.pack (reverse input)
      in (InNormal, [Located p (StringToken str)])

------------------------------------------------------------------------
-- Token builders
------------------------------------------------------------------------

-- | Construct a 'Integer' token from a lexeme.
integer :: Text {- ^ lexeme -} -> Token
integer str = IntegerToken n
  where
  Right (n,_) = Text.signed Text.decimal (Text.filter (/= '_') str)


-- | Construct a 'Double' token from a lexeme.
double :: Text {- ^ lexeme -} -> Token
double str = DoubleToken n
  where
  Right (n,_) = Text.signed Text.double (Text.filter (/= '_') str)


-- | Construct a 'BareKeyToken' for the given lexeme. This operation
-- copies the lexeme into a fresh 'Text' value to ensure that a slice
-- of the original source file is kept.
bareKeyToken :: Text {- ^ lexeme -} -> Token
bareKeyToken txt = BareKeyToken $! Text.copy txt

------------------------------------------------------------------------
-- Date and time token parsers
------------------------------------------------------------------------

-- | Parse a date\/time lexeme to produce a 'Token'. As long as the
-- regular expressions in the "Lexer" module are correct, this parse
-- will never fail, so failure to parse throws an error.
timeParser ::
  ParseTime t =>
  (t -> Token) {- ^ token function   -} ->
  String       {- ^ time format      -} ->
  Text         {- ^ lexeme           -} ->
  Token        {- ^ date\/time token -}
timeParser con fmt txt =
  con (parseTimeOrError False defaultTimeLocale fmt (Text.unpack txt))


-- | Format string for parsing time of day: @hours:minutes:seconds.fractional@
timeFormat :: String
timeFormat = "%T%Q"


-- | Date and time lexeme parsers
zonedtime, localtime, day, timeofday :: Text -> Token
zonedtime = timeParser ZonedTimeToken (iso8601DateFormat (Just timeFormat)++"%Z")
localtime = timeParser LocalTimeToken (iso8601DateFormat (Just timeFormat))
day       = timeParser DayToken       (iso8601DateFormat Nothing)
timeofday = timeParser TimeOfDayToken timeFormat
