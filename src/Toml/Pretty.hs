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
    prettyExpr,
    prettyVal,
    prettyPosition,
    prettySectionKind,

    -- * keys
    prettySimpleKey,
    prettyKey,
    ) where

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Foldable (fold)
import Data.List (intersperse, partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (fromString)
import Data.Time (ZonedTime(zonedTimeZone), TimeZone (timeZoneMinutes))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Prettyprinter
import Text.Printf (printf)
import Toml.Position (Position(..))
import Toml.Raw (Key, SectionKind(..), Expr (..), Val(..))
import Toml.Token (Token(..))
import Toml.Value (Value(..), valueToVal, tableToVal, Table)

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

prettyKey :: Key -> TomlDoc
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

prettySectionKind :: SectionKind -> Key -> TomlDoc
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
    Tok2SquareC         -> "']]]"
    TokCurlyO           -> "'{'"
    TokCurlyC           -> "'}'"
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

prettyExpr :: Expr -> TomlDoc
prettyExpr (KeyValExpr _ k v  ) = prettyAssignment k v
prettyExpr (TableExpr      _ k) = prettySectionKind TableKind k
prettyExpr (ArrayTableExpr _ k) = prettySectionKind ArrayTableKind k

prettyAssignment :: Key -> Val -> TomlDoc
prettyAssignment k v = prettyKey k <+> equals <+> prettyVal v

-- | Render a value suitable for assignment on the right-hand side
-- of an equals sign. This value will always occupy a single line.
prettyValue :: Value -> TomlDoc
prettyValue = prettyVal . valueToVal

prettyVal :: Val -> TomlDoc
prettyVal = \case
    ValInteger i       -> annotate NumberClass (pretty i)
    ValFloat   f
        | isNaN f      -> annotate NumberClass (fromString "nan")
        | isInfinite f -> annotate NumberClass (fromString (if f > 0 then "inf" else "-inf"))
        | otherwise    -> annotate NumberClass (pretty f)
    ValArray a         -> lbracket <> fold (intersperse (fromString ", ") [prettyVal v | v <- a]) <> rbracket
    ValTable t         -> lbrace <> fold (intersperse (fromString ", ") [prettyAssignment k v | (k,v) <- t]) <> rbrace
    ValBool True       -> annotate BoolClass (fromString "true")
    ValBool False      -> annotate BoolClass (fromString "false")
    ValString str      -> annotate StringClass (fromString (quoteString str))
    ValTimeOfDay tod   -> annotate DateClass (fromString (formatTime defaultTimeLocale "%H:%M:%S%Q" tod))
    ValZonedTime zt
      | timeZoneMinutes (zonedTimeZone zt) == 0 ->
                          annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" zt))
      | otherwise      -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" zt))
    ValLocalTime lt    -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" lt))
    ValDay d           -> annotate DateClass (fromString (formatTime defaultTimeLocale "%Y-%m-%d" d))

isAlwaysSimple :: Val -> Bool
isAlwaysSimple = \case
    ValInteger   {} -> True
    ValFloat     {} -> True
    ValBool      {} -> True
    ValString    {} -> True
    ValTimeOfDay {} -> True
    ValZonedTime {} -> True
    ValLocalTime {} -> True
    ValDay       {} -> True
    ValTable     t  -> isSingularTable t
    ValArray     xs -> null xs || not (all isTable xs)

isTable :: Val -> Bool
isTable ValTable{} = True
isTable _       = False

isSingularTable :: [(Key, Val)] -> Bool
isSingularTable [(_, ValTable v)] = isSingularTable v
isSingularTable [(_, v)] = isAlwaysSimple v
isSingularTable _ = False

-- | Render a complete TOML document using top-level table
-- and array of table sections where appropriate.
prettyToml :: Table -> TomlDoc
prettyToml t = prettyToml_ TableKind [] (tableToVal t)

prettyToml_ :: SectionKind -> [String] -> [(Key, Val)] -> TomlDoc
prettyToml_ kind prefix t = vcat (topLines ++ subtables)
    where
        (simple, sections) = partition (isAlwaysSimple . snd) t

        topLines = [fold topElts | let topElts = headers ++ assignments, not (null topElts)]

        headers =
            case NonEmpty.nonEmpty prefix of
                Just key | not (null simple) || null sections || kind == ArrayTableKind ->
                    [prettySectionKind kind key <> hardline]
                _ -> []

        assignments = [prettyAssignment k v <> hardline | (k,v) <- simple]

        subtables = [prettySection (prefix `NonEmpty.prependList` k) v | (k,v) <- sections]

prettySection :: Key -> Val -> TomlDoc
prettySection key (ValTable t) =
    prettyToml_ TableKind (NonEmpty.toList key) t
prettySection key (ValArray a) =
    vcat [prettyToml_ ArrayTableKind (NonEmpty.toList key) t | ValTable t <- a]
prettySection _ _ = error "prettySection applied to simple value"
