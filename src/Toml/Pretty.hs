{-# Language OverloadedStrings, GADTs #-}
{-|
Module      : Toml.Pretty
Description : Human-readable representations for error messages
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides human-readable renderers for types used
in this package to assist error message production.

The generated 'Doc' values are annotated with 'DocClass' values
to assist in producing syntax-highlighted outputs.

To extract a plain String representation, use 'show'.

-}
module Toml.Pretty (
    -- * Types
    TomlDoc,
    DocClass(..),

    -- * Printing semantic values
    prettyToml,
    prettyTomlOrdered,
    prettyValue,

    -- * Printing syntactic components
    prettyToken,
    prettySectionKind,

    -- * Printing keys
    prettySimpleKey,
    prettyKey,
    ) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Foldable (fold)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.String (fromString)
import Data.Time (ZonedTime(zonedTimeZone), TimeZone (timeZoneMinutes))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Prettyprinter
import Text.Printf (printf)
import Toml.Parser (SectionKind(..))
import Toml.Lexer (Token(..))
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

-- | Pretty-printer document with TOML class attributes to aid
-- in syntax-highlighting.
type TomlDoc = Doc DocClass

-- | Renders a dotted-key using quotes where necessary and annotated
-- as a 'KeyClass'.
prettyKey :: NonEmpty String -> TomlDoc
prettyKey = annotate KeyClass . fold . NonEmpty.intersperse dot . fmap prettySimpleKey

-- | Renders a simple-key using quotes where necessary.
prettySimpleKey :: String -> Doc a
prettySimpleKey str
    | not (null str), all isBareKey str = fromString str
    | otherwise                         = fromString (quoteString str)

-- | Predicate for the character-class that is allowed in bare keys
isBareKey :: Char -> Bool
isBareKey x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '-' || x == '_'

-- | Quote a string using basic string literal syntax.
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

-- | Pretty-print a section heading. The result is annotated as a 'TableClass'.
prettySectionKind :: SectionKind -> NonEmpty String -> TomlDoc
prettySectionKind TableKind      key =
    annotate TableClass (unAnnotate (lbracket <> prettyKey key <> rbracket))
prettySectionKind ArrayTableKind key =
    annotate TableClass (unAnnotate (lbracket <> lbracket <> prettyKey key <> rbracket <> rbracket))

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
prettyAssignment = go . pure
    where
        go ks (Table (Map.assocs -> [(k,v)])) = go (NonEmpty.cons k ks) v
        go ks v = prettyKey (NonEmpty.reverse ks) <+> equals <+> prettyValue v

-- | Render a value suitable for assignment on the right-hand side
-- of an equals sign. This value will always use inline table and list
-- syntax.
prettyValue :: Value -> TomlDoc
prettyValue = \case
    Integer i           -> annotate NumberClass (pretty i)
    Float   f
        | isNaN f       -> annotate NumberClass "nan"
        | isInfinite f  -> annotate NumberClass (if f > 0 then "inf" else "-inf")
        | otherwise     -> annotate NumberClass (pretty f)
    Array a             -> align (list [prettyValue v | v <- a])
    Table t             -> lbrace <> concatWith (surround ", ") [prettyAssignment k v | (k,v) <- Map.assocs t] <> rbrace
    Bool True           -> annotate BoolClass "true"
    Bool False          -> annotate BoolClass "false"
    String str          -> annotate StringClass (fromString (quoteString str))
    TimeOfDay tod       -> annotate DateClass (fromString (formatTime defaultTimeLocale "%H:%M:%S%Q" tod))
    ZonedTime zt
        | timeZoneMinutes (zonedTimeZone zt) == 0 ->
                           annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" zt))
        | otherwise     -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" zt))
    LocalTime lt        -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" lt))
    Day d               -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%d" d))

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
isTable _        = False

isSingularTable :: Table -> Bool
isSingularTable (Map.elems -> [v])  = isAlwaysSimple v
isSingularTable _                   = False

-- | Render a complete TOML document using top-level table and array of
-- table sections where possible.
--
-- Keys are sorted alphabetically. To provide a custom ordering, see
-- 'prettyTomlOrdered'.
prettyToml ::
    Table {- ^ table to print -} ->
    TomlDoc {- ^ TOML syntax -}
prettyToml = prettyToml_ NoProjection TableKind []

-- | Render a complete TOML document like 'prettyToml' but use a
-- custom key ordering. The comparison function has access to the
-- complete key path. Note that only keys in the same table will
-- every be compared.
--
-- This operation allows you to render your TOML files with the
-- most important sections first. A TOML file describing a package
-- might desire to have the @[package]@ section first before any
-- of the ancilliary configuration sections.
--
-- The /table path/ is the name of the table being sorted. This allows
-- the projection to be aware of which table is being sorted.
--
-- The /key/ is the key in the table being sorted. These are the
-- keys that will be compared to each other.
--
-- Here's a projection that puts the @package@ section first, the
-- @secondary@ section second, and then all remaining cases are
-- sorted alphabetically afterward.
--
-- @
-- example :: [String] -> String -> Either Int String
-- example [] "package" = Left 1
-- example [] "second"  = Left 2
-- example _  other     = Right other
-- @
--
-- We could also put the tables in reverse-alphabetical order
-- by leveraging an existing newtype.
--
-- @
-- reverseOrderProj :: [String] -> String -> Down String
-- reverseOrderProj _ = Down
-- @
--
-- @since 1.2.1.0
prettyTomlOrdered ::
  Ord a =>
  ([String] -> String -> a) {- ^ table path -> key -> projection -} ->
  Table {- ^ table to print -} ->
  TomlDoc {- ^ TOML syntax -}
prettyTomlOrdered proj = prettyToml_ (KeyProjection proj) TableKind []

-- | Optional projection used to order rendered tables
data KeyProjection where
    -- | No projection provided; alphabetical order used
    NoProjection :: KeyProjection
    -- | Projection provided: table name and current key are available
    KeyProjection :: Ord a => ([String] -> String -> a) -> KeyProjection

prettyToml_ :: KeyProjection -> SectionKind -> [String] -> Table -> TomlDoc
prettyToml_ mbKeyProj kind prefix t = vcat (topLines ++ subtables)
    where
        order =
            case mbKeyProj of
                NoProjection    -> id
                KeyProjection f -> sortOn (f prefix . fst)

        (simple, sections) = partition (isAlwaysSimple . snd) (order (Map.assocs t))

        topLines = [fold topElts | let topElts = headers ++ assignments, not (null topElts)]

        headers =
            case NonEmpty.nonEmpty prefix of
                Just key | not (null simple) || null sections || kind == ArrayTableKind ->
                    [prettySectionKind kind key <> hardline]
                _ -> []

        assignments = [prettyAssignment k v <> hardline | (k,v) <- simple]

        subtables = [prettySection (prefix `snoc` k) v | (k,v) <- sections]

        prettySection key (Table t) =
            prettyToml_ mbKeyProj TableKind (NonEmpty.toList key) t
        prettySection key (Array a) =
            vcat [prettyToml_ mbKeyProj ArrayTableKind (NonEmpty.toList key) t | Table t <- a]
        prettySection _ _ = error "prettySection applied to simple value"

-- | Create a 'NonEmpty' with a given prefix and last element.
snoc :: [a] -> a -> NonEmpty a
snoc []       y = y :| []
snoc (x : xs) y = x :| xs ++ [y]
