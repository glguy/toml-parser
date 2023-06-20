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
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map

import Toml.Pretty (prettyKey, prettySectionKind)
import Toml.Raw (SectionKind(..), Key, Val(..), Expr(..))
import Toml.Value (Value(..))

-- | Extract semantic value from sequence of raw TOML expressions
-- or report an error string.
semantics :: [Expr] -> Either String (Map String Value)
semantics exprs =
 do let (topKVs, tables) = gather exprs
    m1 <- assignKeyVals Map.empty topKVs
    m2 <- foldM (\m (ln, kind, key, kvs) ->
        updateError (\e -> e ++ " in " ++ prettySectionKind kind key ++ " on line " ++ show ln) $
        let update m_ = closeDots <$> assignKeyVals m_ kvs in
        addSection update key kind m) m1 tables
    pure (fmap frameToValue m2)

type KeyVals = [(Int, Key, Val)]

-- | Arrange the expressions in a TOML file into the top-level key-value pairs
-- and then all the key-value pairs for each subtable.
gather :: [Expr] -> (KeyVals, [(Int, SectionKind, Key, KeyVals)])
gather = goTop []
    where
        goTop acc []                              = (reverse acc, [])
        goTop acc (ArrayTableExpr ln key : exprs) = (reverse acc, goTable ln ArrayTableKind key [] exprs)
        goTop acc (TableExpr      ln key : exprs) = (reverse acc, goTable ln TableKind      key [] exprs)
        goTop acc (KeyValExpr     ln k v : exprs) = goTop ((ln,k,v):acc) exprs

        goTable ln kind key acc []                               = (ln, kind, key, reverse acc) : []
        goTable ln kind key acc (TableExpr      ln' k   : exprs) = (ln, kind, key, reverse acc) : goTable ln' TableKind k [] exprs
        goTable ln kind key acc (ArrayTableExpr ln' k   : exprs) = (ln, kind, key, reverse acc) : goTable ln' ArrayTableKind k [] exprs
        goTable ln kind key acc (KeyValExpr     ln' k v : exprs) = goTable ln kind key ((ln',k,v):acc) exprs


updateError :: (e -> e') -> Either e a -> Either e' a
updateError f (Left  e) = Left (f e)
updateError _ (Right a) = Right a

closeDots :: Map String Frame -> Map String Frame
closeDots = fmap closeFrame
    where
        closeFrame = \case
            FrameValue        v -> FrameValue        v
            FrameTable Open   t -> FrameTable Open   t -- don't bother recursing, no further dotted
            FrameTable Dotted t -> FrameTable Closed (closeDots t) -- <- dotted to closed
            FrameTable Closed t -> FrameTable Closed (closeDots t)
            FrameArray        t -> FrameArray        (fmap closeDots t)

assignKeyVals :: Map String Frame -> KeyVals -> Either String (Map String Frame)
assignKeyVals =
    foldM \m (ln,k,v) ->
     do value <- valToValue v
        updateError (\e -> e ++ " while assigning " ++ prettyKey k ++ " on line " ++ show ln)
          (assign k value m)

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

-- | Frames help distinguish tables and arrays written in block and inline
-- syntax. This allows us to enforce that inline tables and arrays can not
-- be extended by block syntax.
data Frame
    = FrameTable FrameKind (Map String Frame)
    | FrameArray (NonEmpty (Map String Frame)) -- stored in reverse order for easy "append"
    | FrameValue Value
    deriving Show

data FrameKind = Open | Dotted | Closed
    deriving Show

frameToValue :: Frame -> Value
frameToValue = \case
    FrameTable _ t -> Table (frameToValue <$> t)
    FrameArray a   -> Array (reverse (Table . fmap frameToValue <$> NonEmpty.toList a))
    FrameValue v   -> v

constructTable :: [(Key, Value)] -> Either String (Map String Value)
constructTable entries =
    case findBadKey (map fst entries) of
        Just bad -> Left ("Overlapping key: " ++ prettyKey bad)
        Nothing -> Right (Map.unionsWith merge [singleValue k ks v | (k:|ks, v) <- entries])
    where
        merge (Table x) (Table y) = Table (Map.unionWith merge x y)
        merge _ _ = error "constructFrame:merge: panic"

        singleValue k []      v = Map.singleton k v
        singleValue k (k1:ks) v = Map.singleton k (Table (singleValue k1 ks v))

-- | Finds a key that overlaps with another in the same list
findBadKey :: [Key] -> Maybe Key
findBadKey keys = check (sort keys)
    where
        check :: [Key] -> Maybe Key
        check (x:y:_)
          | NonEmpty.toList x `NonEmpty.isPrefixOf` y = Just y
        check (_:xs) = check xs
        check [] = Nothing

addSection ::
    (Map String Frame -> Either String (Map String Frame)) {- ^ section update action -} ->
    Key                              {- ^ section key -} ->
    SectionKind                      {- ^ section kind -} ->
    Map String Frame                 {- ^ local frame map -} ->
    Either String (Map String Frame) {- ^ error message or updated local frame map -}

addSection update (key :| []) kind acc = Map.alterF f key acc
    where
        -- defining a new table
        f Nothing =
            case kind of
                TableKind      -> Just . FrameTable Closed    <$> update Map.empty
                ArrayTableKind -> Just . FrameArray . (:| []) <$> update Map.empty

        -- defining a super table of a previously defined subtable
        f (Just (FrameTable Open t)) =
            case kind of
                TableKind      -> Just . FrameTable Closed <$> update t
                ArrayTableKind -> Left "attempt to redefine table as array table"

        -- Add a new array element to an existing table array
        f (Just (FrameArray a)) =
            case kind of
                ArrayTableKind -> Just . FrameArray . (`NonEmpty.cons` a) <$> update Map.empty
                TableKind      -> Left "attempt to open array table as table"

        -- failure cases
        f (Just (FrameTable Closed _)) = Left "attempt to redefine top-level defined table"
        f (Just (FrameTable Dotted _)) = error "addSection: dotted table left unclosed"
        f (Just (FrameValue {}))       = Left "attempt to redefined a value"

addSection update (k1 :| k2 : ks) kind acc = Map.alterF f k1 acc
    where
        go = addSection update (k2 :| ks) kind

        f Nothing                       = Just . FrameTable Open      <$> go Map.empty
        f (Just (FrameTable tk t))      = Just . FrameTable tk        <$> go t
        f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> go t
        f (Just (FrameValue _))         = Left "attempt to redefine a value"


assign :: Key -> Value -> Map String Frame -> Either String (Map String Frame)

assign (key :| []) val acc = Map.alterF f key acc
    where
        f Nothing = Right (Just (FrameValue val))
        f Just{}  = Left "key already assigned"

assign (key:| k1:keys) val acc = Map.alterF f key acc
    where
        go t = Just . FrameTable Dotted <$> assign (k1:|keys) val t

        f Nothing                        = go Map.empty
        f (Just (FrameTable Open     t)) = go t
        f (Just (FrameTable Dotted   t)) = go t
        f (Just (FrameTable Closed   _)) = Left "attempt to extend through a closed table"
        f (Just (FrameArray          _)) = Left "attempt to extend through an array of tables"
        f (Just (FrameValue          _)) = Left "attempted to overwrite a value"
