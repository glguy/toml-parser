module Semantics (compileExprs) where

import Control.Monad (foldM)
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Pretty (prettyKey)
import Raw (Val(..), Expr(..))
import Value (Value(..))

data Section = Section {
    sectionPath :: [String],
    sectionEntries :: [([String], Val)],
    sectionKind :: SectionKind}

data SectionKind = TableKind | ArrayTableKind

compileExprs :: [Expr] -> Either String (Map String Value)
compileExprs exprs = fmap frameToValue <$> combineSections (exprSections exprs)

exprSections :: [Expr] -> [Section]
exprSections = go Section { sectionPath = [], sectionEntries = [], sectionKind = TableKind }
    where
        go s = \case
            [] -> [s]
            KeyValExpr k v : rest ->
                go s { sectionEntries = (k, v) : sectionEntries s } rest
            TableExpr path' : rest ->
                s : go Section { sectionPath = path', sectionEntries = [], sectionKind = TableKind } rest
            ArrayTableExpr path' : rest ->
                s : go Section { sectionPath = path', sectionEntries = [], sectionKind = ArrayTableKind } rest

combineSections :: [Section] -> Either String (Map String Frame)
combineSections [] = error "combineSections expects 1 section"
combineSections (root:xs) =
 do entries <- (traverse . traverse) valToValue (sectionEntries root)
    top <- constructFrame entries
    foldM addSection top xs

addSection :: Map String Frame -> Section -> Either String (Map String Frame)
addSection m s =
    case (traverse . traverse) valToValue (sectionEntries s) of
        Left e -> Left (e ++ " in " ++ prettyKey (sectionPath s))
        Right entries ->
         do tab <- constructFrame entries
            case tryInsert (sectionKind s) (sectionPath s) tab m of
                Left e   -> Left (e ++ " " ++ prettyKey (sectionPath s))
                Right m' -> Right m'

-- | Convert 'Val' to 'Value' potentially raising an error if
-- it has inline tables with key-conflicts.
valToValue :: Val -> Either String Value
valToValue v =
    case v of
      ValInteger i      -> Right (Integer   i)
      ValFloat x        -> Right (Float     x)
      ValBool x         -> Right (Bool      x)
      ValString x       -> Right (String    x)
      ValTimeOfDay x    -> Right (TimeOfDay x)
      ValZonedTime x    -> Right (ZonedTime x)
      ValLocalTime x    -> Right (LocalTime x)
      ValDay x          -> Right (Day       x)
      ValArray xs       -> Array <$> traverse valToValue xs
      ValTable kvs      -> do entries <- (traverse . traverse) valToValue kvs
                              Table <$> constructTable entries

-- | Construct a simple table defined with inline table syntax.
-- This kind of table supports no fancy array-table extension.
constructTable :: [([String], Value)] -> Either String (Map String Value)
constructTable entries = fmap frameToValue <$> constructFrame entries

-- | Frames help distinguish tables and arrays written in block and inline
-- syntax. This allows us to enforce that inline tables and arrays can not
-- be extended by block syntax.
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
        Just bad -> Left ("Overlapping key: " ++ prettyKey bad)
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

tryInsert :: SectionKind -> [String] -> Map String Frame -> Map String Frame -> Either String (Map String Frame)
tryInsert _ [] _ _ = error "tryInsert: empty key"

-- single value insertion, do not overwrite an existing value
tryInsert TableKind [key] val m =
    Map.alterF f key m
    where
        f Nothing = Right (Just (FrameTable val))
        f Just{}  = Left "Duplicate assignment"

-- multiple value insertion, create a new singleton array or extend the existing one
tryInsert ArrayTableKind [key] val m =
    Map.alterF f key m
    where
        f Nothing                 = Right (Just (FrameArray (pure val)))
        f (Just (FrameArray old)) = Right (Just (FrameArray (NonEmpty.cons val old))) -- FrameArray is reversed!
        f _                       = Left "Table/Array-table mismatch"

tryInsert array (key:keys) val m =
    Map.alterF f key m
    where
        f Nothing                 = Just . FrameTable <$> tryInsert array keys val Map.empty
        f (Just (FrameTable old)) = Just . FrameTable <$> tryInsert array keys val old
        f (Just (FrameArray (t:|xs))) =
            Just . FrameArray . (:|xs) <$> tryInsert array keys val t
        f (Just (FrameValue (Table{}))) = Left "Insert into inline table"
        f (Just (FrameValue (Array{}))) = Left "Insert into inline array"
        f (Just (FrameValue _        )) = Left "Insert into simple value"
