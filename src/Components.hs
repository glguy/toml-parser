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
  = TableEntry Path
  | ArrayEntry Path
  | KeyValue Text Value
  deriving (Read, Show)

type Path = [Text]


componentsToTable :: [Component] -> Either Path [(Text,Value)]
componentsToTable = flattenTableList . collapseComponents


collapseComponents :: [Component] -> [(Path,Value)]
collapseComponents [] = []
collapseComponents (KeyValue k v : xs) = ([k],v) : collapseComponents xs
collapseComponents (TableEntry k : xs) =
  case splitKeyValues xs of
    (kvs, xs') -> (k, TableV kvs) : collapseComponents xs'
collapseComponents xs@(ArrayEntry k : _) =
  case splitArrays k xs of
    (kvss, xs') -> (k, ListV (map TableV kvss)) : collapseComponents xs'


splitKeyValues :: [Component] -> ([(Text,Value)], [Component])
splitKeyValues (KeyValue k v : xs) =
  case splitKeyValues xs of
    (kvs, xs') -> ((k,v):kvs, xs')
splitKeyValues xs = ([],xs)


splitArrays :: Path -> [Component] -> ([[(Text,Value)]], [Component])
splitArrays k1 (ArrayEntry k2 : xs)
  | k1 == k2 =
      case splitKeyValues xs of
        (kvs, xs1) ->
          case splitArrays k1 xs1 of
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
    order = sortBy (comparing fst)

    go path xs = sequenceA [ flattenGroup path x ys | (x,ys) <- factorHeads xs ]

    flattenGroup :: Path -> Text -> [(Path,Value)] -> Either Path (Text,Value)
    flattenGroup path k xs | isAmbiguous xs = Left (reverse (k:path))
    flattenGroup path k [([],v)] = (k,v) <$ validateInlineTables (k:path) v
    flattenGroup path k (([],TableV t):kvs) = flattenGroup path k (mergeInlineTable t kvs)
    flattenGroup path k kvs =
      do kvs' <- go (k:path) kvs
         return (k, TableV kvs')

    mergeInlineTable t kvs = order ([([i],j) | (i,j) <- t] ++ kvs)


    isAmbiguous = not . null . drop 1 . takeWhile (null . fst)


validateInlineTables :: Path -> Value -> Either Path ()
validateInlineTables path (TableV t) =
  case findDuplicate (map fst t) of
    Just k -> Left (reverse (k:path))
    Nothing -> traverse_ (\(k,v) -> validateInlineTables (k:path) v) t
validateInlineTables path (ListV xs) =
  zipWithM_ (\i x -> validateInlineTables (Text.pack (show i):path) x) [0::Int ..] xs
validateInlineTables _ _ = Right ()


findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = listToMaybe . map head . filter (not . null . tail) . group . sort
