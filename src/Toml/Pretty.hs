{-|
Module      : Toml.Pretty
Description : Human-readable representations for error messages
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides human-readable renderers for types used
in this package to assist error message production.

-}
module Toml.Pretty (
    -- * semantic values
    prettyToml,
    prettyValue,

    -- * syntactic components
    prettyToken,
    prettyExpr,
    prettyVal,
    prettyPosition,
    prettySectionKind,

    -- * keys
    prettySimpleKey,
    prettyKey,
    ) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.List (intercalate, partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Printf (printf)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Toml.Position (Position(..))
import Toml.Raw (Key, SectionKind(..), Expr (..), Val(..))
import Toml.Token (Token(..))
import Toml.Value (Value(..), valueToVal)

prettyKey :: Key -> String
prettyKey = concat . NonEmpty.intersperse "." . fmap prettySimpleKey

prettySimpleKey :: String -> String
prettySimpleKey str
    | all isBareKey str = str
    | otherwise         = quoteString str

isBareKey :: Char -> Bool
isBareKey x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '-' || x == '_'

quoteString :: String -> String
quoteString = ('"':) . go
    where
        go = \case
            ""        -> "\"" -- terminator
            '"'  : xs -> '\\' : '"'  : go xs
            '\\' : xs -> '\\' : '\\' : go xs
            '\b' : xs -> '\\' : 'b'  : go xs
            '\f' : xs -> '\\' : 'f'  : go xs
            '\n' : xs -> '\\' : 'n'  : go xs
            '\r' : xs -> '\\' : 'r'  : go xs
            '\t' : xs -> '\\' : 't'  : go xs
            x    : xs
                | isPrint x     -> x : go xs
                | x <= '\xffff' -> printf "\\u%04X%s" (ord x) (go xs)
                | otherwise     -> printf "\\U%08X%s" (ord x) (go xs)

prettySectionKind :: SectionKind -> Key -> String
prettySectionKind TableKind      key = "[" ++ prettyKey key ++ "]"
prettySectionKind ArrayTableKind key = "[[" ++ prettyKey key ++ "]]"

prettyPosition :: Position -> String
prettyPosition Position { posIndex = _, posLine = l, posColumn = c } =
    "line " ++ show l ++ " column " ++ show c

-- | Render token for human-readable error messages.
prettyToken :: Token -> String
prettyToken = \case
    TokComma            -> "','"
    TokEquals           -> "'='"
    TokPeriod           -> "'.'"
    TokSquareO          -> "'['"
    TokSquareC          -> "']'"
    Tok2SquareO         -> "'[['"
    Tok2SquareC         -> "']]]"
    TokCurlyO           -> "'{'"
    TokCurlyC           -> "'}'"
    TokComment _        -> "comment"
    TokNewline          -> "newline"
    TokBareKey key      -> "bare key: " ++ key
    TokTrue             -> "true literal"
    TokFalse            -> "false literal"
    TokString str       -> "string: " ++ show str
    TokMlString str     -> "multi-line string: " ++ show str
    TokInteger str      -> "integer: " ++ show str
    TokFloat str        -> "float: " ++ show str
    TokOffsetDateTime _ -> "offset date-time"
    TokLocalDateTime _  -> "local date-time"
    TokLocalDate _      -> "local date"
    TokLocalTime _      -> "local time"
    TokError e          -> "lexical error: " ++ e
    TokEOF              -> "end-of-input"

prettyExpr :: Expr -> String
prettyExpr (KeyValExpr _ k v) = prettyKey k ++ " = " ++ prettyVal v
prettyExpr (TableExpr      _ k) = "["  ++ prettyKey k ++ "]"
prettyExpr (ArrayTableExpr _ k) = "[[" ++ prettyKey k ++ "]]"

-- | Render a value suitable for assignment on the right-hand side
-- of an equals sign. This value will always occupy a single line.
prettyValue :: Value -> String
prettyValue = prettyVal . valueToVal

prettyVal :: Val -> String
prettyVal = \case
    ValInteger i -> show i
    ValFloat   f
        | isNaN f -> "nan"
        | isInfinite f -> if f > 0 then "inf" else "-inf"
        | otherwise -> show f
    ValArray  xs -> "[" ++ intercalate ", " (map prettyVal xs) ++ "]"
    ValTable t   -> "{" ++ intercalate ", " [prettyKey k ++ " = " ++ prettyVal v | (k,v) <- t] ++ "}"
    ValBool True -> "true"
    ValBool False -> "false"
    ValString str -> quoteString str
    ValTimeOfDay tod -> formatTime defaultTimeLocale "%H:%M:%S%Q" tod
    ValZonedTime zt  -> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" zt
    ValLocalTime lt  -> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" lt
    ValDay d         -> formatTime defaultTimeLocale "%Y-%m-%d" d

isAlwaysSimple :: Value -> Bool
isAlwaysSimple = \case
    Integer   {} -> True
    Float     {} -> True
    Bool      {} -> True
    String    {} -> True
    TimeOfDay {} -> True
    ZonedTime {} -> True
    LocalTime {} -> True
    Day       {} -> True
    Table     t  -> isSingularTable t
    Array     xs -> null xs || not (all isTable xs)

isTable :: Value -> Bool
isTable Table{} = True
isTable _       = False

isSingularTable :: Map String Value -> Bool
isSingularTable t =
    case Map.elems t of
        [Table v] -> isSingularTable v
        [v]       -> isAlwaysSimple v
        _         -> False

-- | Render a complete TOML document using top-level table
-- and array of table sections where appropriate.
prettyToml :: Map String Value -> String
prettyToml = prettyToml_ []

prettyToml_ :: [String] -> Map String Value -> String
prettyToml_ prefix t =
    intercalate "\n" $
      [unlines [prettyAssignment (pure k) v | (k,v) <- simple] | not (null simple)] ++
      [prettySection (snoc prefix k) v | (k,v) <- sections]
    where
        snoc [] x = x :| []
        snoc (x:xs) y = x :| (xs ++ [y])
        (simple, sections) =
            partition (isAlwaysSimple . snd) (Map.assocs t)

prettyAssignment :: Key -> Value -> String
prettyAssignment k (Table (Map.assocs -> [(k',v)])) = prettyAssignment (k <> pure k') v
prettyAssignment k v = prettyKey k ++ " = " ++ prettyValue v

prettySection :: Key -> Value -> String
prettySection key (Table t) =
    (if null t || any isAlwaysSimple t then prettySectionKind TableKind key ++ "\n" else "") ++
    prettyToml_ (NonEmpty.toList key) t
prettySection key (Array a) =
    intercalate "\n" [
        prettySectionKind ArrayTableKind key ++ "\n" ++
        prettyToml_ (NonEmpty.toList key) t
        | Table t <- a]
prettySection _ _ = error "prettySection applied to simple value"