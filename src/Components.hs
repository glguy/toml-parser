{-|
Module      : Components
Description : Type and operations for raw top-level TOML elements
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Components where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Foldable
import qualified Data.Text as Text
import Data.Ord
import Data.List
import Value

data Component
  = InitialEntry [(Text,Value)]
  | TableEntry Path [(Text,Value)]
  | ArrayEntry Path [(Text,Value)]
  deriving (Read, Show)

type Path = [Text] -- nonempty


-- | Merge a list of top-level components into a single
-- table, or throw an error with an ambiguous path.
componentsToTable :: [Component] -> Either Path [(Text,Value)]
componentsToTable = flattenTableList . collapseComponents


collapseComponents :: [Component] -> [(Path,Value)]
collapseComponents [] = []
collapseComponents (InitialEntry kvs : xs) =
  [ ([k],v) | (k,v) <- kvs ] ++ collapseComponents xs
collapseComponents (TableEntry k kvs : xs) =
  (k, TableV kvs) : collapseComponents xs
collapseComponents xs@(ArrayEntry k _ : _) =
  case splitArrays k xs of
    (kvss, xs') -> (k, ListV (map TableV kvss)) : collapseComponents xs'


splitArrays :: Path -> [Component] -> ([[(Text,Value)]], [Component])
splitArrays k1 (ArrayEntry k2 kvs : xs)
  | k1 == k2 =
     case splitArrays k1 xs of
       (kvss, xs2) -> (kvs:kvss, xs2)
splitArrays _ xs = ([],xs)


factorHeads :: Eq k => [([k],v)] -> [(k,[([k],v)])]
factorHeads xs = [ (head (fst (head g)),
                   [ (k, v) | (_:k,v) <- g ])
                 | g <- groupBy eq xs
                 ]
  where
    eq x y = head (fst x) == head (fst y)


flattenTableList :: [(Path, Value)] -> Either Path [(Text, Value)]
flattenTableList = go [] . order
  where
    go path xs = sequenceA [ flattenGroup path x ys | (x,ys) <- factorHeads xs ]

    flattenGroup :: Path -> Text -> [(Path,Value)] -> Either Path (Text,Value)
    flattenGroup path k (([],TableV t):kvs) =
      flattenGroup path k (mergeInlineTable t kvs)
    flattenGroup path k (([],v):rest)
      | null rest = (k,v) <$ validateInlineTables (k:path) v
      | otherwise = Left (reverse (k:path))
    flattenGroup path k kvs =
      do kvs' <- go (k:path) kvs
         return (k, TableV kvs')


mergeInlineTable :: [(Text,value)] -> [(Path,value)] -> [(Path,value)]
mergeInlineTable t kvs = order ([([i],j) | (i,j) <- t] ++ kvs)


order :: [(Path,value)] -> [(Path,value)]
order = sortBy (comparing fst)


-- | Throw an error with the problematic path if a duplicate is found.
validateInlineTables :: Path -> Value -> Either Path ()
validateInlineTables path (TableV t) =
  case findDuplicate (map fst t) of
    Just k  -> Left (reverse (k:path))
    Nothing -> traverse_ (\(k,v) -> validateInlineTables (k:path) v) t
validateInlineTables path (ListV xs) =
  zipWithM_ (\i x -> validateInlineTables (Text.pack (show i):path) x)
        [0::Int ..] xs
validateInlineTables _ _ = Right ()


-- | Find an entry that appears in the given list more than once.
findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = listToMaybe . map head . filter (not . null . tail) . group . sort
