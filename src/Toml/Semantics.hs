{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-|
Module      : Toml.Sematics
Description : Semantic interpretation of raw TOML expressions
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module extracts the nested Map representation of a TOML
file. It detects invalid key assignments and resolves dotted
key assignments.

-}
module Toml.Semantics (semantics) where

import Control.Monad (foldM)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Printf (printf)
import Toml.Located (locThing, Located, locPosition)
import Toml.Parser (SectionKind(..), Key, Val(..), Expr(..))
import Toml.Value (Table, Value(..))
import Toml.Position (Position(..))
import Toml.Pretty (prettySimpleKey)
import Control.Applicative ((<|>))

-- | Extract semantic value from sequence of raw TOML expressions
-- or report an error string.
semantics :: [Expr] -> Either String Table
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

-- | Frames help distinguish tables and arrays written in block and inline
-- syntax. This allows us to enforce that inline tables and arrays can not
-- be extended by block syntax.
data Frame
    = FrameTable FrameKind (Map String Frame)
    | FrameArray (NonEmpty (Map String Frame)) -- stored in reverse order for easy "append"
    | FrameValue Value
    deriving Show

data FrameKind
    = Open   -- ^ table implicitly defined as supertable of [x.y.z]
    | Dotted -- ^ table implicitly defined using dotted key assignment
    | Closed -- ^ table closed to further extension
    deriving Show

framesToTable :: Map String Frame -> Table
framesToTable =
    fmap \case
        FrameTable _ t -> Table (framesToTable t)
        FrameArray a   -> Array (toArray a)
        FrameValue v   -> v
    where
        -- reverses the list while converting the frames to tables
        toArray = foldl (\acc frame -> Table (framesToTable frame) : acc) []

constructTable :: [(Key, Value)] -> Either String Table
constructTable entries =
    case findBadKey (map fst entries) of
        Just bad -> invalidKey bad "is already assigned"
        Nothing -> Right (Map.unionsWith merge [singleValue (locThing k) (locThing <$> ks) v | (k:|ks, v) <- entries])
    where
        merge (Table x) (Table y) = Table (Map.unionWith merge x y)
        merge _ _ = error "constructFrame:merge: panic"

        singleValue k []      v = Map.singleton k v
        singleValue k (k1:ks) v = Map.singleton k (Table (singleValue k1 ks v))

-- | Finds a key that overlaps with another in the same list
findBadKey :: [Key] -> Maybe (Located String)
findBadKey = check . sortOn (fmap locThing)
    where
        check :: [Key] -> Maybe (Located String)
        check (x:y:z) = check1 x y <|> check (y:z)
        check _ = Nothing

        check1 (x :| xs) (y1 :| y2 : ys)
            | locThing x == locThing y1 =
                case xs of
                    [] -> Just y1
                    x' : xs' -> check1 (x' :| xs') (y2 :| ys)
        check1 _ _ = Nothing
            



addSection ::
    SectionKind                      {- ^ section kind        -} ->
    KeyVals                          {- ^ values to install   -} ->
    Key                              {- ^ section key         -} ->
    Map String Frame                 {- ^ local frame map     -} ->
    Either String (Map String Frame) {- ^ error message or updated local frame map -}
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
                    ArrayTableKind -> invalidKey k1 "is already a table"

            -- Add a new array element to an existing table array
            Just (FrameArray a) ->
                case kind of
                    ArrayTableKind -> go (FrameArray . (`NonEmpty.cons` a)) Map.empty
                    TableKind      -> invalidKey k1 "is already an array of tables"

            -- failure cases
            Just (FrameTable Closed _) -> invalidKey k1 "is a closed table"
            Just (FrameTable Dotted _) -> error "addSection: dotted table left unclosed"
            Just (FrameValue {})       -> invalidKey k1 "is already assigned"
            where
                go g t = Just . g . closeDots <$> assignKeyVals kvs t

        walk (k1 :| k2 : ks) = flip Map.alterF (locThing k1) \case
            Nothing                     -> go (FrameTable Open     ) Map.empty
            Just (FrameTable tk t)      -> go (FrameTable tk       ) t
            Just (FrameArray (t :| ts)) -> go (FrameArray . (:| ts)) t
            Just (FrameValue _)         -> invalidKey k1 "is already assigned"
            where
                go g t = Just . g <$> walk (k2 :| ks) t

-- | Close all of the tables that were implicitly defined with
-- dotted prefixes.
closeDots :: Map String Frame -> Map String Frame
closeDots =
    fmap \case
        FrameTable Dotted t -> FrameTable Closed (closeDots t)
        frame               -> frame

assignKeyVals :: KeyVals -> Map String Frame -> Either String (Map String Frame)
assignKeyVals kvs t = closeDots <$> foldM f t kvs
    where
        f m (k,v) = assign k v m

-- | Assign a single dotted key in a frame.
assign :: Key -> Val -> Map String Frame -> Either String (Map String Frame)

assign (key :| []) val = flip Map.alterF (locThing key) \case
    Nothing -> Just . FrameValue <$> valToValue val
    Just{}  -> invalidKey key "is already assigned"

assign (key :| k1 : keys) val = flip Map.alterF (locThing key) \case
    Nothing                    -> go Map.empty
    Just (FrameTable Open   t) -> go t
    Just (FrameTable Dotted t) -> go t
    Just (FrameTable Closed _) -> invalidKey key "is a closed table"
    Just (FrameArray        _) -> invalidKey key "is a closed table"
    Just (FrameValue        _) -> invalidKey key "is already assigned"
    where
        go t = Just . FrameTable Dotted <$> assign (k1 :| keys) val t

-- | Convert 'Val' to 'Value' potentially raising an error if
-- it has inline tables with key-conflicts.
valToValue :: Val -> Either String Value
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

invalidKey :: Located String -> String -> Either String a
invalidKey k msg = Left (printf "%d:%d: key error: %s %s"
    (posLine (locPosition k))
    (posColumn (locPosition k))
    (show (prettySimpleKey (locThing k)))
    msg)
