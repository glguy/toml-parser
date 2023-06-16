module LexerUtils where

import Control.Monad.Trans.State (State, modify, state)
import Data.Char (chr, ord, isAscii, isSpace)
import Data.Foldable (asum)
import Data.Time.Format (parseTimeM, defaultTimeLocale, ParseTime)
import Located (Located(..))
import Numeric (readBin, readHex, readOct)
import Position (move)
import Token (Token(..))

type M a = State [Context] a

data Context
  = ListContext  -- processing an inline list, lex values
  | TableContext -- processing an inline table, don't lex values
  | ValueContext -- processing after an equals, lex one value
  deriving Show

pushContext :: Context -> M ()
pushContext cxt = modify \st ->
  case st of
    ValueContext : st' -> cxt : st'
    _                  -> cxt : st

equals :: Action
equals _ = TokEquals <$ pushContext ValueContext

enterList :: Action
enterList _ = TokSquareO <$ pushContext ListContext

enterTable :: Action
enterTable _ = TokCurlyO <$ pushContext TableContext

exitTable :: Action
exitTable _ = state \case
  TableContext : st -> (TokCurlyC             , st)
  st                -> (TokError "Unmatched }", st)

exitList :: Action
exitList _ = state \case
  ListContext : st -> (TokSquareC            , st)
  []               -> (TokSquareC            , [])
  st               -> (TokError "Unmatched ]", st)

token_ :: Token -> Action
token_ t _ = pure t

token :: (String -> Token) -> Action
token f x = pure (f x)

value_ :: Token -> Action
value_ t _ = emitValue t

value :: (String -> Token) -> Action
value f x = emitValue (f x)

emitValue :: a -> M a
emitValue v = state \st ->
  case st of
    ValueContext:st' -> (v, st')
    _                -> (v, st )

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

timeValue :: ParseTime a => String -> [String] -> (a -> Token) -> Action
timeValue description patterns constructor = value \str ->
  case asum [parseTimeM False defaultTimeLocale pattern str | pattern <- patterns] of
    Nothing -> TokError ("Malformed " ++ description)
    Just t  -> constructor t

type AlexInput = Located String

alexGetByte :: AlexInput -> Maybe (Int, AlexInput)
alexGetByte Located { locPosition = p, locThing = str } =
  case str of
    "" -> Nothing
    x:xs
      | x == '\1' -> Just (0,     rest)
      | isAscii x -> Just (ord x, rest)
      | otherwise -> Just (1,     rest)
      where
        rest = Located { locPosition = move x p, locThing = xs }

type Action = String -> M Token

-------------------

processDec :: String -> Integer
processDec ('+':xs) = read xs
processDec xs = read xs

processHex :: String -> Integer
processHex ('0':'x':xs) = fst (head (readHex xs))
processHex _ = error "processHex: bad input"

processOct :: String -> Integer
processOct ('0':'o':xs) = fst (head (readOct xs))
processOct _ = error "processHex: bad input"

processBin :: String -> Integer
processBin ('0':'b':xs) = fst (head (readBin xs))
processBin _ = error "processHex: bad input"

processFloat :: String -> Double
processFloat "nan" = 0/0
processFloat "+nan" = 0/0
processFloat "-nan" = 0/0
processFloat "inf" = 1/0
processFloat "+inf" = 1/0
processFloat "-inf" = -1/0
processFloat ('+':x) = read x
processFloat x = read x

processLiteral :: String -> String
processLiteral = tail . init

processBasic :: String -> String
processBasic "" = error "processBasic: missing initializer"
processBasic (_:start) = go start
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

processMlBasic :: String -> String
processMlBasic str =
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

processMlLiteral :: String -> String
processMlLiteral str =
  case str of
    '\'':'\'':'\'':'\r':'\n':start -> go start
    '\'':'\'':'\'':'\n':start -> go start
    '\'':'\'':'\'':start -> go start
    _ -> error "processMlLiteral: mising initializer"
  where
    go "'''" = ""
    go (x:xs) = x : go xs
    go "" = error "processMlLiteral: missing terminator"
