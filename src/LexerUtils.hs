{-# LANGUAGE BangPatterns #-}
{-|
Module      : LexerUtils
Description : Lexer support operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module is separate from the Lexer.x input to Alex
to segregate the automatically generated code from the
hand written code. The automatically generated code
causes lots of warnings which mask the interesting warnings.
-}
module LexerUtils where

import Data.Char            (isSpace, isControl, isAscii, ord, chr)
import Data.Foldable        (asum)
import Data.Text            (Text)
import Data.Time
import Data.Word            (Word8)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Tokens

------------------------------------------------------------------------
-- Custom Alex wrapper - these functions are used by generated code
------------------------------------------------------------------------

-- | The generated code expects the lexer input type to be named 'AlexInput'
type AlexInput = Located Text

-- | Get the next characteristic byte from the input source.
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (c,cs') <- Text.uncons cs
       let !b = byteForChar c
           !inp = Located (move p c) cs'
       return (b, inp)

------------------------------------------------------------------------

-- | The initial 'Position' for the start of a file
startPos :: Position
startPos = Position { posIndex = 0, posLine = 1, posColumn = 1 }

-- | Advance the position according to the kind of character lexed.
move :: Position -> Char -> Position
move (Position ix line column) c =
  case c of
    '\t' -> Position (ix + 1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> Position (ix + 1) (line + 1) 1
    _    -> Position (ix + 1) line (column + 1)

-- | Action to perform upon end of file. Produce errors if EOF was unexpected.
eofAction :: Position -> LexerMode -> [Located Token]
eofAction eofPosn st =
  case st of
    InString _ posn _ -> [Located posn (Error UntermString)]
    InNormal          -> [Located eofPosn EOF]

-- | Action to perform when lexer gets stuck. Emits an error.
errorAction :: AlexInput -> [Located Token]
errorAction inp = [fmap (Error . NoMatch . Text.head) inp]

------------------------------------------------------------------------
-- Lexer Modes
------------------------------------------------------------------------

-- | The lexer can be in any of four modes which determine which rules
-- are active.
data LexerMode
  = InNormal
  | InString !Int !Position String
  deriving Show

-- | Type of actions used by lexer upon matching a rule
type Action =
  Located Text                 {- ^ located lexeme                     -} ->
  LexerMode                    {- ^ lexer mode                         -} ->
  (LexerMode, [Located Token]) {- ^ updated lexer mode, emitted tokens -}

-- | Helper function for building an 'Action' using the lexeme
token :: (Text -> Token) -> Action
token f match st = (st, [fmap f match])

-- | Helper function for building an 'Action' where the lexeme is unused.
token_ :: Token -> Action
token_ = token . const

------------------------------------------------------------------------
-- Alternative modes
------------------------------------------------------------------------

-- | Enter the string literal lexer
startString :: Int -> Action
startString mode lexeme _ = (InString mode (locPosition lexeme) [], [])

emitChar :: Action
emitChar _ InNormal = error "PANIC: emitChar used in normal mode"
emitChar lexeme (InString mode pos acc) = (InString mode pos acc', [])
  where
    acc' = reverse (Text.unpack (locThing lexeme)) ++ acc

emitChar' :: Char -> Action
emitChar' c _ (InString mode pos acc) = (InString mode pos (c : acc), [])
emitChar' _ _ _ = error "PANIC: emitChar' used in normal mode"


emitUnicodeChar :: Action
emitUnicodeChar lexeme mode =
  case Text.hexadecimal (Text.drop 2 (locThing lexeme)) of
    Right (n, _)
      | n < 0x110000 -> emitChar' (chr n) lexeme mode
      | otherwise    -> (InNormal, [Located (locPosition lexeme) (Error BadEscape)])
    _ -> error "PANIC: bad unicode unescape implementation"


-- | Successfully terminate the current mode and emit tokens as needed
endString :: Action
endString _ mode =
  case mode of
    InNormal -> error "PANIC: error in string literal lexer"
    InString _ p input ->
      (InNormal, [Located p (String (Text.pack (reverse input)))])

------------------------------------------------------------------------
-- Token builders
------------------------------------------------------------------------

-- | Construct a 'Integer' token from a lexeme.
integer :: Text {- ^ lexeme -} -> Token
integer str = Integer n
  where
  Right (n,_) = Text.signed Text.decimal (Text.filter (/= '_') str)

-- | Construct a 'Double' token from a lexeme.
double :: Text {- ^ lexeme -} -> Token
double str = Double n
  where
  Right (n,_) = Text.signed Text.double (Text.filter (/= '_') str)


-- | The TOML format doesn't distinguish between any of the non-ASCII
-- characters. This function extracts the printable and whitespace
-- subset of Unicode and maps it to the ASCII value as used by Alex.
byteForChar :: Char -> Word8
byteForChar c
  | isControl c && not (isSpace c) = 0
  | isAscii c = fromIntegral (ord c)
  | otherwise = 0

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

zonedtime, localtime, day, timeofday :: Text -> Token
zonedtime = timeParser ZonedTimeTok (iso8601DateFormat (Just timeFormat)++"%Z")
localtime = timeParser LocalTimeTok (iso8601DateFormat (Just timeFormat))
day       = timeParser DayTok       (iso8601DateFormat Nothing)
timeofday = timeParser TimeOfDayTok timeFormat
