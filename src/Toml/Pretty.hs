{-# Language OverloadedStrings #-}
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
    -- * Types
    TomlDoc,
    DocClass(..),

    -- * semantic values
    prettyToml,
    prettyToml_,
    prettyValue,

    -- * syntactic components
    prettyToken,
    prettyPosition,
    prettySectionKind,

    -- * keys
    prettySimpleKey,
    prettyKey,
    ) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Foldable (fold)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Time (ZonedTime(zonedTimeZone), TimeZone (timeZoneMinutes))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Prettyprinter
import Text.Printf (printf)
import Toml.Position (Position(..))
import Toml.Raw (SectionKind(..))
import Toml.Token (Token(..))
import Toml.Value (Value(..), Table)

-- | Annotation used to enable styling pretty-printed TOML
data DocClass
    = TableClass  -- ^ top-level @[key]@ and @[[key]]@
    | KeyClass    -- ^ dotted keys, left-hand side of assignments
    | StringClass -- ^ string literals
    | NumberClass -- ^ number literals
    | DateClass   -- ^ date and time literals
    | BoolClass   -- ^ boolean literals
    deriving (Read, Show, Eq, Ord)

type TomlDoc = Doc DocClass

prettyKey :: NonEmpty String -> TomlDoc
prettyKey = annotate KeyClass . fold . NonEmpty.intersperse dot . fmap prettySimpleKey

prettySimpleKey :: String -> Doc a
prettySimpleKey str
    | not (null str), all isBareKey str = fromString str
    | otherwise                         = fromString (quoteString str)

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

prettySectionKind :: SectionKind -> NonEmpty String -> TomlDoc
prettySectionKind TableKind      key =
    annotate TableClass (unAnnotate (lbracket <> prettyKey key <> rbracket))
prettySectionKind ArrayTableKind key =
    annotate TableClass (unAnnotate (lbracket <> lbracket <> prettyKey key <> rbracket <> rbracket))

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
    Tok2SquareC         -> "']]'"
    TokCurlyO           -> "'{'"
    TokCurlyC           -> "'}'"
    TokNewline          -> "newline"
    TokBareKey        _ -> "bare key"
    TokTrue             -> "true literal"
    TokFalse            -> "false literal"
    TokString         _ -> "string"
    TokMlString       _ -> "multi-line string"
    TokInteger        _ -> "integer"
    TokFloat          _ -> "float"
    TokOffsetDateTime _ -> "offset date-time"
    TokLocalDateTime  _ -> "local date-time"
    TokLocalDate      _ -> "local date"
    TokLocalTime      _ -> "local time"
    TokError          e -> "lexical error: " ++ e
    TokEOF              -> "end-of-input"

prettyAssignment :: String -> Value -> TomlDoc
prettyAssignment = go . NonEmpty.singleton
    where
        go ks (Table (Map.assocs -> [(k,v)])) = go (NonEmpty.cons k ks) v
        go ks v = prettyKey (NonEmpty.reverse ks) <+> equals <+> prettyValue v

-- | Render a value suitable for assignment on the right-hand side
-- of an equals sign. This value will always occupy a single line.
prettyValue :: Value -> TomlDoc
prettyValue = \case
    Integer i       -> annotate NumberClass (pretty i)
    Float   f
        | isNaN f      -> annotate NumberClass "nan"
        | isInfinite f -> annotate NumberClass (if f > 0 then "inf" else "-inf")
        | otherwise    -> annotate NumberClass (pretty f)
    Array a         -> align (list [prettyValue v | v <- a])
    Table t         -> lbrace <> concatWith (surround ", ") [prettyAssignment k v | (k,v) <- Map.assocs t] <> rbrace
    Bool True       -> annotate BoolClass "true"
    Bool False      -> annotate BoolClass "false"
    String str      -> annotate StringClass (fromString (quoteString str))
    TimeOfDay tod   -> annotate DateClass (fromString (formatTime defaultTimeLocale "%H:%M:%S%Q" tod))
    ZonedTime zt
      | timeZoneMinutes (zonedTimeZone zt) == 0 ->
                          annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" zt))
      | otherwise      -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" zt))
    LocalTime lt    -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" lt))
    Day d           -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%d" d))

isAlwaysSimple :: Value -> Bool
isAlwaysSimple = \case
    Integer   _ -> True
    Float     _ -> True
    Bool      _ -> True
    String    _ -> True
    TimeOfDay _ -> True
    ZonedTime _ -> True
    LocalTime _ -> True
    Day       _ -> True
    Table     x -> isSingularTable x
    Array     x -> null x || not (all isTable x)

isTable :: Value -> Bool
isTable Table {} = True
isTable _           = False

isSingularTable :: Table -> Bool
isSingularTable (Map.elems -> [Table v]) = isSingularTable v
isSingularTable (Map.elems -> [v])       = isAlwaysSimple v
isSingularTable _                        = False

-- | Render a complete TOML document using top-level table
-- and array of table sections where appropriate.
prettyToml :: Table -> TomlDoc
prettyToml = prettyToml_ TableKind []

prettyToml_ :: SectionKind -> [String] -> Table -> TomlDoc
prettyToml_ kind prefix t = vcat (topLines ++ subtables)
    where
        (simple, sections) = partition (isAlwaysSimple . snd) (Map.assocs t)

        topLines = [fold topElts | let topElts = headers ++ assignments, not (null topElts)]

        headers =
            case NonEmpty.nonEmpty prefix of
                Just key | not (null simple) || null sections || kind == ArrayTableKind ->
                    [prettySectionKind kind key <> hardline]
                _ -> []

        assignments = [prettyAssignment k v <> hardline | (k,v) <- simple]

        subtables = [prettySection (prefix `NonEmpty.prependList` pure k) v | (k,v) <- sections]

prettySection :: NonEmpty String -> Value -> TomlDoc
prettySection key (Table t) =
    prettyToml_ TableKind (NonEmpty.toList key) t
prettySection key (Array a) =
    vcat [prettyToml_ ArrayTableKind (NonEmpty.toList key) t | Table t <- a]
prettySection _ _ = error "prettySection applied to simple value"
