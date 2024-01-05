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
import Data.List.NonEmpty qualified as NonEmpty
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
semantics :: [Expr] -> M Table
semantics exprs =
 do let (topKVs, tables) = gather exprs
    m1 <- assignKeyVals topKVs Map.empty
    m2 <- foldM (\m (kind, key, kvs) ->
        addSection kind kvs key m) m1 tables
    pure (framesToTable m2)

-- | Line number, key, value
type KeyVals = [(Key, Val)]

-- | Arrange the expressions in a TOML file into the top-level key-value pairs
-- and then all the key-value pairs for each subtable.
gather :: [Expr] -> (KeyVals, [(SectionKind, Key, KeyVals)])
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
-- and array resolution is still happening.
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
        FrameTable _ t -> Table (framesToTable t)
        FrameArray a   -> Array (toArray a)
        FrameValue v   -> v
    where
        -- reverses the list while converting the frames to tables
        toArray = foldl (\acc frame -> Table (framesToTable frame) : acc) []

-- | Build a 'Table' value out of a list of key-value pairs. These keys are
-- checked to not overlap. In the case of overlap a 'SemanticError' is returned.
constructTable :: [(Key, Value)] -> M Table
constructTable = foldM (uncurry . addEntry) Map.empty
    where
        -- turns x.y.z = v into a nested table of one leaf value
        singleCase = foldr (\k v -> Table (Map.singleton (locThing k) v))

        addEntry tab (key :| subkey) val = Map.alterF f (locThing key) tab
            where
                -- no existing assignment at this parent key - no more validation needed
                f Nothing = pure (Just (singleCase val subkey))

                -- there's already a table at this parent key, attempt to extend it
                f (Just (Table subtab)) | Just subkey' <- NonEmpty.nonEmpty subkey =
                    Just . Table <$> addEntry subtab subkey' val

                -- attempted to overwrite an existing assignment, abort
                f _ = invalidKey key AlreadyAssigned

-- | Attempts to insert the key-value pairs given into a new section
-- located at the given key-path in a frame map.
addSection ::
    SectionKind  {- ^ section kind                               -} ->
    KeyVals      {- ^ values to install                          -} ->
    Key          {- ^ section key                                -} ->
    FrameTable   {- ^ local frame map                            -} ->
    M FrameTable {- ^ error message or updated local frame table -}
addSection kind kvs = walk
    where
        walk (k1 :| []) = flip Map.alterF (locThing k1) \case
            -- defining a new table
            Nothing ->
                case kind of
                    TableKind      -> go (FrameTable Closed) Map.empty
                    ArrayTableKind -> go (FrameArray . pure) Map.empty

            -- defining a super table of a previously defined subtable
            Just (FrameTable Open t) ->
                case kind of
                    TableKind      -> go (FrameTable Closed) t
                    ArrayTableKind -> invalidKey k1 ImplicitlyTable

            -- Add a new array element to an existing table array
            Just (FrameArray a) ->
                case kind of
                    ArrayTableKind -> go (FrameArray . (`NonEmpty.cons` a)) Map.empty
                    TableKind      -> invalidKey k1 ClosedTable

            -- failure cases
            Just (FrameTable Closed _) -> invalidKey k1 ClosedTable
            Just (FrameTable Dotted _) -> error "addSection: dotted table left unclosed"
            Just (FrameValue {})       -> invalidKey k1 AlreadyAssigned
            where
                go g t = Just . g . closeDots <$> assignKeyVals kvs t

        walk (k1 :| k2 : ks) = flip Map.alterF (locThing k1) \case
            Nothing                     -> go (FrameTable Open     ) Map.empty
            Just (FrameTable tk t)      -> go (FrameTable tk       ) t
            Just (FrameArray (t :| ts)) -> go (FrameArray . (:| ts)) t
            Just (FrameValue _)         -> invalidKey k1 AlreadyAssigned
            where
                go g t = Just . g <$> walk (k2 :| ks) t

-- | Close all of the tables that were implicitly defined with
-- dotted prefixes. These tables are only eligible for extension
-- within the @[table]@ section in which they were introduced.
closeDots :: FrameTable -> FrameTable
closeDots =
    fmap \case
        FrameTable Dotted t -> FrameTable Closed (closeDots t)
        frame               -> frame

-- | Extend the given frame table with a list of key-value pairs.
-- Either the updated frame table will be returned
assignKeyVals :: KeyVals -> FrameTable -> M FrameTable
assignKeyVals kvs t = closeDots <$> foldM f t kvs
    where
        f m (k,v) = assign k v m

-- | Assign a single dotted key in a frame.
assign :: Key -> Val -> FrameTable -> M FrameTable

assign (key :| []) val = flip Map.alterF (locThing key) \case
    Nothing -> Just . FrameValue <$> valToValue val
    Just{}  -> invalidKey key AlreadyAssigned

assign (key :| k1 : keys) val = flip Map.alterF (locThing key) \case
    Nothing                    -> go Map.empty
    Just (FrameTable Open   t) -> go t
    Just (FrameTable Dotted t) -> go t
    Just (FrameTable Closed _) -> invalidKey key ClosedTable
    Just (FrameArray        _) -> invalidKey key ClosedTable
    Just (FrameValue        _) -> invalidKey key AlreadyAssigned
    where
        go t = Just . FrameTable Dotted <$> assign (k1 :| keys) val t

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
    ValTable kvs      -> do entries <- (traverse . traverse) valToValue kvs
                            Table <$> constructTable entries

-- | Abort validation by reporting an error about the given key.
invalidKey ::
    Located String    {- ^ subkey     -} ->
    SemanticErrorKind {- ^ error kind -} ->
    M a
invalidKey key kind = Left ((`SemanticError` kind) <$> key)
