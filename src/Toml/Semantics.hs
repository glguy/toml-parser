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
        addSection kind kvs ln key m) m1 tables
    pure (fmap frameToValue m2)

-- | Line number, key, value
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

data FrameKind
    = Open   -- ^ table implicitly defined as supertable of [x.y.z]
    | Dotted -- ^ table implicitly defined using dotted key assignment
    | Closed -- ^ table closed to further extension
    deriving Show

frameToValue :: Frame -> Value
frameToValue = \case
    FrameTable _ t -> Table (frameToValue <$> t)
    FrameArray a   -> Array (reverse (Table . fmap frameToValue <$> NonEmpty.toList a))
    FrameValue v   -> v

constructTable :: [(Key, Value)] -> Either String (Map String Value)
constructTable entries =
    case findBadKey (map fst entries) of
        Just bad -> Left ("Overlapping key: " ++ show (prettyKey bad))
        Nothing -> Right (Map.unionsWith merge [singleValue k ks v | (k:|ks, v) <- entries])
    where
        merge (Table x) (Table y) = Table (Map.unionWith merge x y)
        merge _ _ = error "constructFrame:merge: panic"

        singleValue k []      v = Map.singleton k v
        singleValue k (k1:ks) v = Map.singleton k (Table (singleValue k1 ks v))

-- | Finds a key that overlaps with another in the same list
findBadKey :: [Key] -> Maybe Key
findBadKey = check . sort
    where
        check :: [Key] -> Maybe Key
        check (x:y:_)
          | NonEmpty.toList x `NonEmpty.isPrefixOf` y = Just y
        check (_:xs) = check xs
        check [] = Nothing

addSection ::
    SectionKind                      {- ^ section kind        -} ->
    KeyVals                          {- ^ values to install   -} ->
    Int                              {- ^ section line number -} ->
    Key                              {- ^ section key         -} ->
    Map String Frame                 {- ^ local frame map     -} ->
    Either String (Map String Frame) {- ^ error message or updated local frame map -}

addSection kind kvs ln topkey = walk topkey
    where
        failure e = Left (e ++ " in " ++ show (prettySectionKind kind topkey) ++ " on line " ++ show ln)

        walk (key :| []) = Map.alterF f key
            where
                update t = closeDots <$> assignKeyVals t kvs

                -- defining a new table
                f Nothing =
                    case kind of
                        TableKind      -> Just . FrameTable Closed    <$> update Map.empty
                        ArrayTableKind -> Just . FrameArray . (:| []) <$> update Map.empty

                -- defining a super table of a previously defined subtable
                f (Just (FrameTable Open t)) =
                    case kind of
                        TableKind      -> Just . FrameTable Closed <$> update t
                        ArrayTableKind -> failure "attempt to redefine table as array table"

                -- Add a new array element to an existing table array
                f (Just (FrameArray a)) =
                    case kind of
                        ArrayTableKind -> Just . FrameArray . (`NonEmpty.cons` a) <$> update Map.empty
                        TableKind      -> failure "attempt to open array table as table"

                -- failure cases
                f (Just (FrameTable Closed _)) = failure "attempt to redefine top-level defined table"
                f (Just (FrameTable Dotted _)) = error "addSection: dotted table left unclosed"
                f (Just (FrameValue {}))       = failure "attempt to redefined a value"
        
        walk (k1 :| k2 : ks) = Map.alterF f k1
            where
                walk' = walk (k2 :| ks)

                f Nothing                       = Just . FrameTable Open      <$> walk' Map.empty
                f (Just (FrameTable tk t))      = Just . FrameTable tk        <$> walk' t
                f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> walk' t
                f (Just (FrameValue _))         = failure "attempt to redefine a value"

-- | Close all of the tables that were implicitly defined with
-- dotted prefixes.
closeDots :: Map String Frame -> Map String Frame
closeDots =
    fmap \case
        FrameTable Dotted t -> FrameTable Closed (closeDots t)
        frame               -> frame

assignKeyVals :: Map String Frame -> KeyVals -> Either String (Map String Frame)
assignKeyVals t kvs = closeDots <$> foldM f t kvs
    where
        f m (ln,k,v) =
            updateError (\e -> e ++ " while assigning " ++ show (prettyKey k) ++ " on line " ++ show ln)
                (assign k v m)

-- | Assign a single dotted key in a frame.
assign :: Key -> Val -> Map String Frame -> Either String (Map String Frame)

assign (key :| []) val = Map.alterF f key
    where
        f Nothing = Just . FrameValue <$> valToValue val
        f Just{}  = Left "key already assigned"

assign (key:| k1:keys) val = Map.alterF f key
    where
        go t = Just . FrameTable Dotted <$> assign (k1 :| keys) val t

        f Nothing                        = go Map.empty
        f (Just (FrameTable Open     t)) = go t
        f (Just (FrameTable Dotted   t)) = go t
        f (Just (FrameTable Closed   _)) = Left "attempt to extend through a closed table"
        f (Just (FrameArray          _)) = Left "attempt to extend through an array of tables"
        f (Just (FrameValue          _)) = Left "attempted to overwrite a value"

updateError :: (e -> e') -> Either e a -> Either e' a
updateError f (Left  e) = Left (f e)
updateError _ (Right a) = Right a
