{-|
Module      : Toml.Semantics.Ordered
Description : Tool for extracting an ordering from an existing TOML file
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module can help build a key ordering projection given an existing
TOML file. This could be useful for applying a transformation to a TOML
file before pretty-printing it back in something very close to the
original order.

When using the computed order, table keys will be remembered in the order
they appeared in the source file. Any key additional keys added to the
tables will be ordered alphabetically after all the known keys.

@
demo =
 do txt <- 'readFile' \"demo.toml\"
    let Right exprs = 'Toml.Parser.parseRawToml' txt
        to          = 'extractTableOrder' exprs
        Right toml  = 'Toml.Semantics.semantics' exprs
        projection  = 'projectKey' to
    'print' ('Toml.Pretty.prettyTomlOrdered' projection toml)
@

@since 1.3.1.0

-}
module Toml.Semantics.Ordered (
    TableOrder,
    extractTableOrder,
    projectKey,
    ProjectedKey,
    debugTableOrder,
    ) where

import Data.Foldable (foldl', toList)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Toml.Parser.Types (Expr(..), Key, Val(ValTable, ValArray))

-- | Summary of the order of the keys in a TOML document.
newtype TableOrder = TO (Map String KeyOrder)

-- | Internal type used by 'TableOrder'
--
-- The 'Int' field determines the order of the current key and the
-- 'TableOrder' determines the order of the children of this key.
data KeyOrder = KeyOrder !Int TableOrder

-- | Opaque type used by 'projectKey'
newtype ProjectedKey = PK (Either Int String)
    deriving (Eq, Ord)

-- | Generate a projection function for use with 'Toml.Pretty.prettyTomlOrdered'
projectKey ::
    TableOrder   {- ^ table order                           -} ->
    [String]     {- ^ table path                            -} ->
    String       {- ^ key                                   -} ->
    ProjectedKey {- ^ type suitable for ordering table keys -}
projectKey (TO to) [] = \k ->
    case Map.lookup k to of
        Just (KeyOrder i _)     -> PK (Left i)
        Nothing                 -> PK (Right k)
projectKey (TO to) (p:ps) =
    case Map.lookup p to of
        Just (KeyOrder _ to')   -> projectKey to' ps
        Nothing                 -> PK . Right

emptyOrder :: TableOrder
emptyOrder = TO Map.empty

-- | Extract a 'TableOrder' from the output of 'Toml.Parser.parseRawToml'
-- to be later used with 'projectKey'.
extractTableOrder :: [Expr a] -> TableOrder
extractTableOrder = snd . foldl' addExpr ([], emptyOrder)

addExpr :: ([String], TableOrder) -> Expr a -> ([String], TableOrder)
addExpr (prefix, to) = \case
    TableExpr k      -> let k' = keyPath k in (k', addKey to k')
    ArrayTableExpr k -> let k' = keyPath k in (k', addKey to k')
    KeyValExpr k v   -> (prefix, addVal prefix (addKey to (prefix ++ keyPath k)) v)

addVal :: [String] -> TableOrder -> Val a -> TableOrder
addVal prefix to lval =
    case lval of
        ValArray _ xs -> foldl' (addVal prefix) to xs
        ValTable _ kvs ->
            foldl' (\acc (k,v) ->
                let k' = prefix ++ keyPath k in
                addVal k' (addKey acc k') v) to kvs
        _ -> to

addKey :: TableOrder -> [String] -> TableOrder
addKey to [] = to
addKey (TO to) (x:xs) = TO (Map.alter f x to)
    where
        f Nothing = Just (KeyOrder (Map.size to) (addKey emptyOrder xs))
        f (Just (KeyOrder i m)) = Just (KeyOrder i (addKey m xs))

keyPath :: Key a -> [String]
keyPath = map snd . toList

-- | Render a white-space nested representation of the key ordering extracted
-- by 'extractTableOrder'. This is provided for debugging and understandability.
debugTableOrder :: TableOrder -> String
debugTableOrder to = unlines (go 0 to [])
    where
        go i (TO m) z =
            foldr (go1 i) z
                (sortOn p (Map.assocs m))

        go1 i (k, KeyOrder _ v) z =
            (replicate (4*i) ' ' ++ k) :
            go (i+1) v z

        p (_, KeyOrder i _) = i
