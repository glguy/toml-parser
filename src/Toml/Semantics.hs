{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-|
Module      : Toml.Semantics
Description : Semantic interpretation of raw TOML expressions
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module extracts the nested Map representation of a TOML
file. It detects invalid key assignments and resolves dotted
key assignments.

-}
module Toml.Semantics (SemanticError(..), SemanticErrorKind(..), semantics) where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Toml.Located (locThing, Located)
import Toml.Parser.Types (SectionKind(..), Key, Val(..), Expr(..))
import Toml.Value (Table, Value(..))

-- | The type of errors that can be generated when resolving all the keys
-- used in a TOML document. These errors always pertain to some key that
-- caused one of three conflicts.
--
-- @since 1.3.0.0
data SemanticError = SemanticError {
    errorKey :: String,
    errorKind :: SemanticErrorKind
    } deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

-- | Enumeration of the kinds of conflicts a key can generate.
--
-- @since 1.3.0.0
data SemanticErrorKind
    = AlreadyAssigned -- ^ Attempted to assign to a key that was already assigned
    | ClosedTable     -- ^ Attempted to open a table already closed
    | ImplicitlyTable -- ^ Attempted to open a tables as an array of tables that was implicitly defined to be a table
    deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

-- | Extract semantic value from sequence of raw TOML expressions
-- or report a semantic error.
--
-- @since 1.3.0.0
semantics :: [Expr] -> Either (Located SemanticError) Table
semantics exprs =
 do let (topKVs, tables) = gather exprs
    m1 <- assignKeyVals topKVs mempty
    m2 <- foldM (\m (kind, key, kvs) ->
        addSection kind kvs key m) m1 tables
    pure (framesToTable m2)

-- | Arrange the expressions in a TOML file into the top-level key-value pairs
-- and then all the key-value pairs for each subtable.
gather :: [Expr] -> ([(Key, Val)], [(SectionKind, Key, [(Key, Val)])])
gather = goTop []
    where
        goTop acc []                           = (reverse acc, [])
        goTop acc (ArrayTableExpr key : exprs) = (reverse acc, goTable ArrayTableKind key [] exprs)
        goTop acc (TableExpr      key : exprs) = (reverse acc, goTable TableKind      key [] exprs)
        goTop acc (KeyValExpr     k v : exprs) = goTop ((k,v):acc) exprs

        goTable kind key acc []                           = (kind, key, reverse acc) : []
        goTable kind key acc (TableExpr      k   : exprs) = (kind, key, reverse acc) : goTable TableKind k [] exprs
        goTable kind key acc (ArrayTableExpr k   : exprs) = (kind, key, reverse acc) : goTable ArrayTableKind k [] exprs
        goTable kind key acc (KeyValExpr     k v : exprs) = goTable kind key ((k,v):acc) exprs

-- | A top-level table used to distinguish top-level defined arrays
-- and tables from inline values.
type FrameTable = Map String Frame

-- | M is the error-handling monad used through this module for
-- propagating semantic errors through the 'semantics' function.
type M = Either (Located SemanticError)

-- | Frames are the top-level skeleton of the TOML file that mirror the
-- subset of values that can be constructed with with top-level syntax.
-- TOML syntax makes a distinction between tables and arrays that are
-- defined at the top-level and those defined with inline syntax. This
-- separate type keeps these syntactic differences separate while table
-- and array resolution is still happening. Frames can keep track of which
-- tables finished and which are eligible for extension.
data Frame
    = FrameTable FrameKind FrameTable
    | FrameArray (NonEmpty FrameTable) -- stored in reverse order for easy "append"
    | FrameValue Value
    deriving Show

-- | Top-level tables can be in various states of completeness. This type
-- keeps track of the current state of a top-level defined table.
data FrameKind
    = Open   -- ^ table implicitly defined as supertable of [x.y.z]
    | Dotted -- ^ table implicitly defined using dotted key assignment
    | Closed -- ^ table closed to further extension
    deriving Show

-- | Convert a top-level table "frame" representation into the plain Value
-- representation once the distinction is no longer needed.
framesToTable :: FrameTable -> Table
framesToTable =
    fmap \case
        FrameTable _ t       -> framesToValue t
        FrameArray (t :| ts) -> Array (rev (map framesToValue (t : ts)))
        FrameValue v         -> v
    where
        rev = foldl (flip (:)) [] -- GHC fails to inline reverse

-- | Convert 'FrameTable' to a 'Value' forgetting all of the
-- frame distinctions.
framesToValue :: FrameTable -> Value
framesToValue = Table . framesToTable

-- | Attempts to insert the key-value pairs given into a new section
-- located at the given key-path in a frame map.
addSection ::
    SectionKind  {- ^ section kind                               -} ->
    [(Key, Val)] {- ^ values to install                          -} ->
    Key          {- ^ section key                                -} ->
    FrameTable   {- ^ local frame map                            -} ->
    M FrameTable {- ^ error message or updated local frame table -}
addSection kind kvs = walk
    where
        walk (k :| []) = alterFrame k \case
            -- defining a new table
            Nothing ->
                case kind of
                    TableKind      -> FrameTable Closed <$> go mempty
                    ArrayTableKind -> FrameArray . pure <$> go mempty

            -- defining a super table of a previously defined subtable
            Just (FrameTable Open t) ->
                case kind of
                    TableKind      -> FrameTable Closed <$> go t
                    ArrayTableKind -> invalidKey k ImplicitlyTable

            -- Add a new array element to an existing table array
            Just (FrameArray (t :| ts)) ->
                case kind of
                    TableKind      -> invalidKey k ClosedTable
                    ArrayTableKind -> FrameArray . (:| t : ts) <$> go mempty

            -- failure cases
            Just (FrameTable Closed _) -> invalidKey k ClosedTable
            Just (FrameTable Dotted _) -> error "addSection: dotted table left unclosed"
            Just (FrameValue {})       -> invalidKey k AlreadyAssigned
            where
                go = assignKeyVals kvs

        walk (k1 :| k2 : ks) = alterFrame k1 \case
            Nothing                     -> FrameTable Open      <$> go mempty
            Just (FrameTable tk t)      -> FrameTable tk        <$> go t
            Just (FrameArray (t :| ts)) -> FrameArray . (:| ts) <$> go t
            Just (FrameValue _)         -> invalidKey k1 AlreadyAssigned
            where
                go = walk (k2 :| ks)

-- | Close all of the tables that were implicitly defined with
-- dotted prefixes. These tables are only eligible for extension
-- within the @[table]@ section in which they were introduced.
closeDots :: FrameTable -> FrameTable
closeDots =
    fmap \case
        FrameTable Dotted t -> FrameTable Closed (closeDots t)
        frame               -> frame

-- | Extend the given frame table with a list of key-value pairs.
-- Any tables created through dotted keys will be closed after
-- all of the key-value pairs and processed.
assignKeyVals :: [(Key, Val)] -> FrameTable -> M FrameTable
assignKeyVals kvs t = closeDots <$> foldM f t kvs
    where
        f m (k,v) = assign k v m

-- | Assign a single dotted key in a frame. Any table traversed
-- by a dotted key will be marked as dotted so that it will become
-- closed at the end of the current call to 'assignKeyVals'.
assign :: Key -> Val -> FrameTable -> M FrameTable

assign (key :| []) val = alterFrame key \case
    Nothing -> FrameValue <$> valToValue val
    Just{}  -> invalidKey key AlreadyAssigned

assign (key :| k1 : keys) val = alterFrame key \case
    Nothing                    -> go mempty
    Just (FrameTable Open   t) -> go t
    Just (FrameTable Dotted t) -> go t
    Just (FrameTable Closed _) -> invalidKey key ClosedTable
    Just (FrameArray        _) -> invalidKey key ClosedTable
    Just (FrameValue        _) -> invalidKey key AlreadyAssigned
    where
        go t = FrameTable Dotted <$> assign (k1 :| keys) val t

-- | Convert 'Val' to 'Value' potentially raising an error if
-- it has inline tables with key-conflicts.
valToValue :: Val -> M Value
valToValue = \case
    ValInteger   x    -> Right (Integer   x)
    ValFloat     x    -> Right (Float     x)
    ValBool      x    -> Right (Bool      x)
    ValString    x    -> Right (String    x)
    ValTimeOfDay x    -> Right (TimeOfDay x)
    ValZonedTime x    -> Right (ZonedTime x)
    ValLocalTime x    -> Right (LocalTime x)
    ValDay       x    -> Right (Day       x)
    ValArray xs       -> Array <$> traverse valToValue xs
    ValTable kvs      -> framesToValue <$> assignKeyVals kvs mempty

-- | Abort validation by reporting an error about the given key.
invalidKey ::
    Located String    {- ^ subkey     -} ->
    SemanticErrorKind {- ^ error kind -} ->
    M a
invalidKey key kind = Left ((`SemanticError` kind) <$> key)

-- | Specialization of 'Map.alterF' used to adjust a location in a 'FrameTable'
alterFrame :: Located String -> (Maybe Frame -> M Frame) -> FrameTable -> M FrameTable
alterFrame k f = Map.alterF (fmap Just . f) (locThing k)
