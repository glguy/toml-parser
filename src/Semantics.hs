module Semantics (compileExprs) where

import Raw ( Val(..), Expr(..) )
import Value ( Value(..) )
import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))


data Section = Section {
    sectionPath :: [String],
    sectionEntries :: [([String], Val)],
    sectionCard :: Cardinality}

data Cardinality = Single | Multiple

compileExprs :: [Expr] -> Either String (Map String Value)
compileExprs exprs = fmap frameToValue <$> combineSections (exprSections exprs)

exprSections :: [Expr] -> [Section]
exprSections = go Section { sectionPath = [], sectionEntries = [], sectionCard = Single }
    where
        go s xs =
            case xs of
                []                          -> [s]
                KeyValExpr     k v   : rest -> go s { sectionEntries = (k, v) : sectionEntries s } rest
                TableExpr      path' : rest -> s : go Section { sectionPath = path', sectionEntries = [], sectionCard = Single   } rest
                ArrayTableExpr path' : rest -> s : go Section { sectionPath = path', sectionEntries = [], sectionCard = Multiple } rest

combineSections :: [Section] -> Either String (Map String Frame)
combineSections [] = error "combineSections expects 1 section"
combineSections (x:xs) =
 do entries <- (traverse . traverse) valToValue (sectionEntries x)
    top <- constructFrame entries
    foldM addSection top xs

addSection :: Map String Frame -> Section -> Either String (Map String Frame)
addSection m s =
 do entries <- (traverse . traverse) valToValue (sectionEntries s)
    tab <- constructFrame entries
    case tryInsert (sectionCard s) (sectionPath s) tab m of
        Nothing -> Left ("Duplicate table: " ++ show (sectionPath s))
        Just m' -> Right m'

valToValue :: Val -> Either String Value
valToValue v =
    case v of
      ValInteger i      -> Right (Integer i)
      ValFloat x        -> Right (Float x)
      ValBool x         -> Right (Bool x)
      ValString x       -> Right (String x)
      ValTimeOfDay x    -> Right (TimeOfDay x)
      ValZonedTime x    -> Right (ZonedTime x)
      ValLocalTime x    -> Right (LocalTime x)
      ValDay x          -> Right (Day x)
      ValArray xs       -> Array <$> traverse valToValue xs
      ValTable kvs      -> do entries <- (traverse . traverse) valToValue kvs
                              Table <$> constructTable entries

-- | Construct a simple table defined with inline table syntax.
-- This kind of table supports no fancy array-table extension.
constructTable :: [([String], Value)] -> Either String (Map String Value)
constructTable entries = fmap frameToValue <$> constructFrame entries

data Frame
    = FrameTable (Map String Frame)
    | FrameArray (NonEmpty (Map String Frame)) -- stored in reverse order for easy "append"
    | FrameValue Value
    deriving Show

frameToValue :: Frame -> Value
frameToValue = \case
    FrameTable t -> Table (frameToValue <$> t)
    FrameArray a -> Array (reverse (Table . fmap frameToValue <$> NonEmpty.toList a))
    FrameValue v -> v

constructFrame :: [([String], Value)] -> Either String (Map String Frame)
constructFrame entries =
    case findBadKey (map fst entries) of
        Just bad -> Left ("Overlapping key: " ++ show bad)
        Nothing -> Right (Map.unionsWith merge [singleValue ks (FrameValue v) | (ks, v) <- entries])
    where
        merge (FrameTable x) (FrameTable y) = FrameTable (Map.unionWith merge x y)
        merge _ _ = error "constructFrame:merge: panic"

        singleValue [k]    v = Map.singleton k v
        singleValue (k:ks) v = Map.singleton k (FrameTable (singleValue ks v))
        singleValue []     _ = error "singleValue: bad empty key"

findBadKey :: [[String]] -> Maybe [String]
findBadKey keys = check (sort keys)
    where
        check (x:y:_)
          | x `isPrefixOf` y = Just y
        check (_:xs) = check xs
        check [] = Nothing

tryInsert :: Cardinality -> [String] -> Map String Frame -> Map String Frame -> Maybe (Map String Frame)
tryInsert _ [] _ _ = error "tryInsert: empty key"

-- single value insertion, do not overwrite an existing value
tryInsert Single [key] val m =
    Map.alterF f key m
    where
        f Nothing = Just (Just (FrameTable val))
        f Just{}  = Nothing

-- multiple value insertion, create a new singleton array or extend the existing one
tryInsert Multiple [key] val m =
    Map.alterF f key m
    where
        f Nothing                 = Just (Just (FrameArray (pure val)))
        f (Just (FrameArray old)) = Just (Just (FrameArray (NonEmpty.cons val old))) -- FrameArray is reversed!
        f _                       = Nothing

tryInsert array (key:keys) val m =
    Map.alterF f key m
    where
        f Nothing                 = Just . FrameTable <$> tryInsert array keys val Map.empty
        f (Just (FrameTable old)) = Just . FrameTable <$> tryInsert array keys val old
        f (Just (FrameArray (t:|xs))) =
            Just . FrameArray . (:|xs) <$> tryInsert array keys val t
        f _ = Nothing
