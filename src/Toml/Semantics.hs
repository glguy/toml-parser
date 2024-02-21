{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-|
Module      : Toml.Semantics
Description : Semantic interpretation of raw TOML expressions
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module extracts a nested Map representation of a TOML
file. It detects invalid key assignments and resolves dotted
key assignments.

-}
module Toml.Semantics (

    -- * Types
    Value, Value'(..),
    Table, Table'(..),

    -- * Validation
    semantics,
    SemanticError(..), SemanticErrorKind(..),

    -- * Annotations
    forgetTableAnns,
    forgetValueAnns,
    valueAnn,
    valueType,

    ) where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Toml.Syntax.Types (SectionKind(..), Key, Val(..), Expr(..))
import Toml.Semantics.Types

-- | This type represents errors generated when resolving keys in a TOML
-- document.
--
-- @since 1.3.0.0
data SemanticError a = SemanticError {
    errorAnn :: a, -- ^ Annotation associated with offending key
    errorKey :: Text,
    errorKind :: SemanticErrorKind
    } deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -},
        Functor, Foldable, Traversable)

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

-- | Extracts a semantic value from a sequence of raw TOML expressions,
-- or reports a semantic error if one occurs.
--
-- @since 1.3.0.0
semantics :: [Expr a] -> Either (SemanticError a) (Table' a)
semantics exprs =
 do f <- foldM processExpr (flip assignKeyVals Map.empty) exprs
    framesToTable <$> f []
    where
        processExpr f = \case
            KeyValExpr   k v -> Right (f . ((k,v):))
            TableExpr      k -> processSection TableKind      k
            ArrayTableExpr k -> processSection ArrayTableKind k
            where
                processSection kind k = flip (addSection kind k) <$> f []

-- | A top-level table used to distinguish top-level defined arrays
-- and tables from inline values.
type FrameTable a = Map Text (a, Frame a)

-- | M is the error-handling monad used through this module for
-- propagating semantic errors through the 'semantics' function.
type M a = Either (SemanticError a)

-- | Frames are the top-level skeleton of the TOML file that mirror the
-- subset of values that can be constructed with with top-level syntax.
-- TOML syntax makes a distinction between tables and arrays that are
-- defined at the top-level and those defined with inline syntax. This
-- separate type keeps these syntactic differences separate while table
-- and array resolution is still happening. Frames can keep track of which
-- tables finished and which are eligible for extension.
data Frame a
    = FrameTable a FrameKind (FrameTable a)
    | FrameArray (NonEmpty (a, FrameTable a)) -- stored in reverse order for easy "append"
    | FrameValue (Value' a)
    deriving Show

-- | Top-level tables can be in various states of completeness. This type
-- keeps track of the current state of a top-level defined table.
data FrameKind
    = Open   -- ^ table implicitly defined as super-table of [x.y.z]
    | Dotted -- ^ table implicitly defined using dotted key assignment
    | Closed -- ^ table closed to further extension
    deriving Show

-- | Convert a top-level table "frame" representation into the plain Value
-- representation once the distinction is no longer needed.
framesToTable :: FrameTable a -> Table' a
framesToTable = fmap MkTable $ fmap $ fmap
    \case
        FrameTable a _kind t -> Table' a (framesToTable t)
        FrameArray (NonEmpty.reverse -> t :| ts) ->
            -- the array itself is attributed to the first table defined
            List' (fst t) [Table' a (framesToTable x) | (a, x) <- t : ts]
        FrameValue v -> v

-- | Attempts to insert the key-value pairs given into a new section
-- located at the given key-path in a frame map.
addSection ::
    SectionKind      {- ^ section kind                           -} ->
    Key a            {- ^ section key                            -} ->
    [(Key a, Val a)] {- ^ values to install                      -} ->
    FrameTable a     {- ^ local frame map                        -} ->
    M a (FrameTable a) {- ^ error message or updated local frame table -}

addSection kind (k :| []) kvs =
    alterFrame k
        -- defining a new table
        (case kind of
                TableKind      -> FrameTable (fst k) Closed <$> go mempty
                ArrayTableKind -> FrameArray . (:| []) . (,) (fst k) <$> go mempty)

        \case
        -- defining a super table of a previously defined sub-table
        FrameTable _ Open t ->
            case kind of
                -- the annotation of the open table changes from the first mention closing key
                TableKind      -> FrameTable (fst k) Closed <$> go t
                ArrayTableKind -> invalidKey k ImplicitlyTable

        -- Add a new array element to an existing table array
        FrameArray (t :| ts) ->
            case kind of
                TableKind      -> invalidKey k ClosedTable
                ArrayTableKind -> FrameArray . (:| t : ts) . (,) (fst k) <$> go mempty

        -- failure cases
        FrameTable _ Closed _ -> invalidKey k ClosedTable
        FrameTable _ Dotted _ -> error "addSection: dotted table left unclosed"
        FrameValue {}         -> invalidKey k AlreadyAssigned
        where
            go = assignKeyVals kvs

addSection kind (k1 :| k2 : ks) kvs =
    alterFrame k1
        (FrameTable (fst k1) Open      <$> go mempty)
        \case
        FrameTable a tk t    -> FrameTable a tk      <$> go t
        FrameArray (t :| ts) -> FrameArray . (:| ts) <$> traverse go t
        FrameValue _         -> invalidKey k1 AlreadyAssigned
        where
            go = addSection kind (k2 :| ks) kvs

-- | Close all of the tables that were implicitly defined with
-- dotted prefixes. These tables are only eligible for extension
-- within the @[table]@ section in which they were introduced.
closeDots :: FrameTable a -> FrameTable a
closeDots =
    fmap $ fmap \case
        FrameTable a Dotted t -> FrameTable a Closed (closeDots t)
        frame                 -> frame

-- | Extend the given frame table with a list of key-value pairs.
-- Any tables created through dotted keys will be closed after
-- all of the key-value pairs are processed.
assignKeyVals :: [(Key a, Val a)] -> FrameTable a -> M a (FrameTable a)
assignKeyVals kvs t = closeDots <$> foldM f t kvs
    where
        f m (k,v) = assign k v m

-- | Assign a single dotted key in a frame. Any open table traversed
-- by a dotted key will be marked as dotted so that it will become
-- closed at the end of the current call to 'assignKeyVals'.
assign :: Key a -> Val a -> FrameTable a -> M a (FrameTable a)

assign (key :| []) val =
    alterFrame key
        (FrameValue <$> valToValue val)
        (\_ -> invalidKey key AlreadyAssigned)

assign (key :| k1 : keys) val =
    alterFrame key (go (fst key) mempty)
        \case
        FrameTable a Open   t -> go a t
        FrameTable a Dotted t -> go a t
        FrameTable _ Closed _ -> invalidKey key ClosedTable
        FrameArray          _ -> invalidKey key ClosedTable
        FrameValue          _ -> invalidKey key AlreadyAssigned
    where
        go a t = FrameTable a Dotted <$> assign (k1 :| keys) val t

-- | Convert 'Val' to 'Value' potentially raising an error if
-- it contains inline tables with key-conflicts.
valToValue :: Val a -> M a (Value' a)
valToValue =
    \case
        ValInteger   a x    -> Right (Integer'   a x)
        ValFloat     a x    -> Right (Double'    a x)
        ValBool      a x    -> Right (Bool'      a x)
        ValString    a x    -> Right (Text'      a x)
        ValTimeOfDay a x    -> Right (TimeOfDay' a x)
        ValZonedTime a x    -> Right (ZonedTime' a x)
        ValLocalTime a x    -> Right (LocalTime' a x)
        ValDay       a x    -> Right (Day'       a x)
        ValArray     a xs   -> List' a <$> traverse valToValue xs
        ValTable     a kvs  -> Table' a . framesToTable <$> assignKeyVals kvs mempty

-- | Abort validation by reporting an error about the given key.
invalidKey ::
    (a, Text)         {- ^ sub-key    -} ->
    SemanticErrorKind {- ^ error kind -} ->
    M a b
invalidKey (a, key) kind = Left (SemanticError a key kind)

-- | Specialization of 'Map.alterF' used to adjust a location in a 'FrameTable'
alterFrame ::
    (a, Text)                  {- ^ annotated key     -} ->
    M a (Frame a)              {- ^ new value case    -} ->
    (Frame a -> M a (Frame a)) {- ^ update value case -} ->
    FrameTable a -> M a (FrameTable a)
alterFrame (a, k) create update = Map.alterF g k
    where
        -- insert a new value
        g Nothing =
            do lf <- create
               pure (Just (a, lf))

        -- update an existing value and preserve its annotation
        g (Just (op, ov)) =
            do lf <- update ov
               pure (Just (op, lf))
