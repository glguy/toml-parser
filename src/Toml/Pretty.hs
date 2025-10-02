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

    -- * Locations
    prettyLocated,
    prettyPosition,
    ) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Foldable (fold, toList)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered qualified as OMap
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (ZonedTime(zonedTimeZone), TimeZone (timeZoneMinutes))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Prettyprinter
import Text.Printf (printf)
import Toml.Semantics
import Toml.Syntax.Lexer (Token(..))
import Toml.Syntax.Position (Located(..), Position(..))
import Toml.Syntax.Types (SectionKind(..))

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
prettyKey :: NonEmpty Text -> TomlDoc
prettyKey = annotate KeyClass . fold . NonEmpty.intersperse dot . fmap prettySimpleKey

-- | Renders a simple-key using quotes where necessary.
prettySimpleKey :: Text -> Doc a
prettySimpleKey str
    | not (Text.null str), Text.all isBareKey str = pretty str
    | otherwise = fromString (quoteString (Text.unpack str))

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

-- | Quote a string using basic string literal syntax.
quoteMlString :: String -> String
quoteMlString = ("\"\"\"\n"++) . go
    where
        go = \case
            "" -> "\"\"\"" -- terminator
            '"' : '"' : '"' : xs -> "\"\"\\\"" ++ go xs
            '\\' : xs -> '\\' : '\\' : go xs
            '\b' : xs -> '\\' : 'b' : go xs
            '\f' : xs -> '\\' : 'f' : go xs
            '\t' : xs -> '\\' : 't' : go xs
            '\n' : xs -> '\n' : go xs
            '\r' : '\n' : xs -> '\r' : '\n' : go xs
            '\r' : xs -> '\\' : 'r' : go xs
            x    : xs
                | isPrint x     -> x : go xs
                | x <= '\xffff' -> printf "\\u%04X%s" (ord x) (go xs)
                | otherwise     -> printf "\\U%08X%s" (ord x) (go xs)

-- | Pretty-print a section heading. The result is annotated as a 'TableClass'.
prettySectionKind :: SectionKind -> NonEmpty Text -> TomlDoc
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
    TokNewline          -> "end-of-line"
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
    TokEOF              -> "end-of-input"

prettyAssignment :: Text -> Value' l -> TomlDoc
prettyAssignment = go . pure
    where
        go ks (Table' _ (MkTable (OMap.assocs -> [(k,(_, v))]))) = go (NonEmpty.cons k ks) v
        go ks v = prettyKey (NonEmpty.reverse ks) <+> equals <+> prettyValue v

-- | Render a value suitable for assignment on the right-hand side
-- of an equals sign. This value will always use inline table and list
-- syntax.
prettyValue :: Value' l -> TomlDoc
prettyValue = \case
    Integer' _ i           -> annotate NumberClass (pretty i)
    Double' _   f
        | isNaN f       -> annotate NumberClass "nan"
        | isInfinite f  -> annotate NumberClass (if f > 0 then "inf" else "-inf")
        | otherwise     -> annotate NumberClass (pretty f)
    List' _ a           -> align (list [prettyValue v | v <- a])
    Table' _ (MkTable t) -> lbrace <> concatWith (surround ", ") [prettyAssignment k v | (k,(_, v)) <- OMap.assocs t] <> rbrace
    Bool' _ True        -> annotate BoolClass "true"
    Bool' _ False       -> annotate BoolClass "false"
    Text' _ str         -> prettySmartString str
    TimeOfDay' _ tod    -> annotate DateClass (fromString (formatTime defaultTimeLocale "%H:%M:%S%Q" tod))
    ZonedTime' _ zt
        | timeZoneMinutes (zonedTimeZone zt) == 0 ->
                           annotate DateClass (fromString (formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%QZ" zt))
        | otherwise     -> annotate DateClass (fromString (formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%Q%Ez" zt))
    LocalTime' _ lt     -> annotate DateClass (fromString (formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M:%S%Q" lt))
    Day' _ d            -> annotate DateClass (fromString (formatTime defaultTimeLocale "%0Y-%m-%d" d))

prettySmartString :: Text -> TomlDoc
prettySmartString str
    | '\n' `elem` Text.unpack str = -- Text.elem isn't in text-1.2
        column \i ->
        pageWidth \case
            AvailablePerLine n _ | Text.length str > n - i ->
                prettyMlString str
            _ -> prettyString str
    | otherwise = prettyString str

prettyMlString :: Text -> TomlDoc
prettyMlString str = annotate StringClass (column \i -> hang (-i) (fromString (quoteMlString (Text.unpack str))))

prettyString :: Text -> TomlDoc
prettyString str = annotate StringClass (fromString (quoteString (Text.unpack str)))

-- | Predicate for values that CAN rendered on the
-- right-hand side of an @=@.
isSimple :: Value' l -> Bool
isSimple = \case
    Integer'   {} -> True
    Double'    {} -> True
    Bool'      {} -> True
    Text'      {} -> True
    TimeOfDay' {} -> True
    ZonedTime' {} -> True
    LocalTime' {} -> True
    Day'       {} -> True
    Table' _    x -> isSingularTable x -- differs from isAlwaysSimple
    List'  _    x -> null x || not (all isTable x)

-- | Predicate for values that can be MUST rendered on the
-- right-hand side of an @=@.
isAlwaysSimple :: Value' l -> Bool
isAlwaysSimple = \case
    Integer'   {} -> True
    Double'    {} -> True
    Bool'      {} -> True
    Text'      {} -> True
    TimeOfDay' {} -> True
    ZonedTime' {} -> True
    LocalTime' {} -> True
    Day'       {} -> True
    Table'     {} -> False -- differs from isSimple
    List' _     x -> null x || not (all isTable x)

-- | Predicate for table values.
isTable :: Value' l -> Bool
isTable Table'{} = True
isTable _        = False

-- | Predicate for tables that can be rendered with a single assignment.
-- These can be collapsed using dotted-key notation on the left-hand side
-- of a @=@.
isSingularTable :: Table' l -> Bool
isSingularTable (MkTable (toList -> [(_, v)])) = isSimple v
isSingularTable _ = False

-- | Render a complete TOML document using top-level table and array of
-- table sections where possible.
--
-- Keys preserve table order. To provide a custom ordering, see
-- 'prettyTomlOrdered'.
prettyToml ::
    Table' a {- ^ table to print -} ->
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
-- of the ancillary configuration sections.
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
prettyTomlOrdered ::
  Ord a =>
  ([Text] -> Text -> a) {- ^ table path -> key -> projection -} ->
  Table' l {- ^ table to print -} ->
  TomlDoc {- ^ TOML syntax -}
prettyTomlOrdered proj = prettyToml_ (KeyProjection proj) TableKind []

-- | Optional projection used to order rendered tables
data KeyProjection where
    -- | No projection provided; preserve existing table order
    NoProjection :: KeyProjection
    -- | Projection provided: table name and current key are available
    KeyProjection :: Ord a => ([Text] -> Text -> a) -> KeyProjection

prettyToml_ :: KeyProjection -> SectionKind -> [Text] -> Table' l -> TomlDoc
prettyToml_ mbKeyProj kind prefix (MkTable t) = vcat (topLines ++ subtables)
    where
        order =
            case mbKeyProj of
                NoProjection    -> id
                KeyProjection f -> sortOn (f prefix . fst)

        kvs = order (OMap.assocs t)

        -- this table will require no subsequent tables to be defined
        simpleToml = all (isSimple . snd) t

        (simple, sections) = partition (isAlwaysSimple . snd . snd) kvs

        topLines = [fold topElts | let topElts = headers ++ assignments, not (null topElts)]

        headers =
            case NonEmpty.nonEmpty prefix of
                Just key | simpleToml || not (null simple) || null sections || kind == ArrayTableKind ->
                    [prettySectionKind kind key <> hardline]
                _ -> []

        assignments = [prettyAssignment k v <> hardline | (k,(_, v)) <- if simpleToml then kvs else simple]

        subtables = [prettySection (prefix ++ [k]) v | not simpleToml, (k,(_, v)) <- sections]

        prettySection key (Table' _ tab) =
            prettyToml_ mbKeyProj TableKind key tab
        prettySection key (List' _ a) =
            vcat [prettyToml_ mbKeyProj ArrayTableKind key tab | Table' _ tab <- a]
        prettySection _ _ = error "prettySection applied to simple value"

-- | Pretty-print as @line:col: message@
prettyLocated :: Located String -> String
prettyLocated (Located p s) = printf "%s: %s" (prettyPosition p) s

-- | Pretty-print as @line:col@
prettyPosition :: Position -> String
prettyPosition p = printf "%d:%d" (posLine p) (posColumn p)
