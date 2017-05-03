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
  | InString !StringMode !Position String
  deriving Show

data StringMode = SLDQ | SLSQ | MLDQ | MLSQ
  deriving Show

-- | Type of actions used by lexer upon matching a rule
type Action =
  Int                          {- ^ match length                       -} ->
  Located Text                 {- ^ current input                      -} ->
  LexerMode                    {- ^ lexer mode                         -} ->
  (LexerMode, [Located Token]) {- ^ updated lexer mode, emitted tokens -}

-- | Helper function for building an 'Action' using the lexeme
token :: (Text -> Token) -> Action
token f len match st = (st, [fmap (f . Text.take len) match])

-- | Helper function for building an 'Action' where the lexeme is unused.
token_ :: Token -> Action
token_ = token . const

------------------------------------------------------------------------
-- Alternative modes
------------------------------------------------------------------------

-- | Enter the string literal lexer
startString :: StringMode -> Action
startString mode _ input _ = (InString mode (locPosition input) [], [])

emitChar :: Action
emitChar len input = emitChar' (Text.head (locThing input)) len input

emitChar' :: Char -> Action
emitChar' c _ _ (InString mode pos acc) = (InString mode pos (c : acc), [])
emitChar' _ _ _ _ = error "PANIC: emitChar' used in normal mode"

emitShortUnicode :: Action
emitShortUnicode len input =
  case Text.hexadecimal (Text.take 4 (Text.drop 2 (locThing input))) of
    Right (n, _) -> emitChar' (chr n) len input
    _ -> error "PANIC: bad short unicode implementation"

emitLongUnicode :: Action
emitLongUnicode len input =
  case Text.hexadecimal (Text.take 8 (Text.drop 2 (locThing input))) of
    Right (n, _) -> emitChar' (chr n) len input
    _ -> error "PANIC: bad short unicode implementation"

-- | Successfully terminate the current mode and emit tokens as needed
endString :: Action
endString _ _ mode =
  case mode of
    InNormal -> error "PANIC: error in toml lexer"
    InString _ p input ->
      (InNormal, [Located p (String (Text.pack (reverse input)))])

------------------------------------------------------------------------
-- Token builders
------------------------------------------------------------------------

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
integer :: Text {- ^ sign-digits -} -> Token
integer str = Integer n
  where
  Right (n,_) = Text.signed Text.decimal (Text.filter (/= '_') str)

double :: Text {- ^ sign-digits -} -> Token
double str = Double n
  where
  Right (n,_) = Text.signed Text.double (Text.filter (/= '_') str)

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | isControl c && not (isSpace c) = 0
  | isAscii c   = fromIntegral (ord c)
  | otherwise   = 0

timeParser :: ParseTime t => [String] -> Text -> t
timeParser fmts txt = t
  where
    Just t = asum [parseTimeM False defaultTimeLocale fmt (Text.unpack txt) | fmt <- fmts]

zonedtime :: Text -> Token
zonedtime = ZonedTimeTok . timeParser
  [ iso8601DateFormat (Just "%H:%M:%S") ++ extra ++ "%Z"
  | extra  <- ["%Q",""] ]

localtime :: Text -> Token
localtime = LocalTimeTok . timeParser
  [ iso8601DateFormat (Just "%H:%M:%S") ++ extra
  | extra <- ["%Q",""]]

day :: Text -> Token
day = DayTok . timeParser [ iso8601DateFormat Nothing ]

timeofday :: Text -> Token
timeofday = TimeOfDayTok . timeParser
  [ "%H:%M:%S" ++ extra | extra <- ["%Q",""]]
