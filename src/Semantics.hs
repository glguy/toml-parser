module Semantics (compileExprs) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Pretty (prettyKey)
import Raw (Key, Val(..), Expr(..))
import Value (Value(..))

data SectionKind = TableKind | ArrayTableKind

compileExprs :: [Expr] -> Either String (Map String Value)
compileExprs exprs = fmap frameToValue <$> build [] Map.empty exprs

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

-- | Frames help distinguish tables and arrays written in block and inline
-- syntax. This allows us to enforce that inline tables and arrays can not
-- be extended by block syntax.
data Frame
    = FrameTable FrameKind (Map String Frame)
    | FrameArray (NonEmpty (Map String Frame)) -- stored in reverse order for easy "append"
    | FrameValue Value
    deriving Show

data FrameKind = Implicit | Defined
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

findBadKey :: [Key] -> Maybe Key
findBadKey keys = check (sort keys)
    where
        check :: [Key] -> Maybe Key
        check (x:y:_)
          | NonEmpty.toList x `NonEmpty.isPrefixOf` y = Just y
        check (_:xs) = check xs
        check [] = Nothing

build :: [String] -> Map String Frame -> [Expr] -> Either String (Map String Frame)

build _ acc [] = Right acc

build _ acc (TableExpr key : exprs) =
 do acc' <- addTop key TableKind acc
    build (NonEmpty.toList key) acc' exprs

build _ acc (ArrayTableExpr key : exprs) =
 do acc' <- addTop key ArrayTableKind acc
    build (NonEmpty.toList key) acc' exprs

build prefix acc (KeyValExpr key val : exprs) =
 do value <- valToValue val
    acc' <- assign prefix key value acc
    build prefix acc' exprs

addTop :: Key -> SectionKind -> Map String Frame -> Either String (Map String Frame)

addTop (key :| []) kind acc = Map.alterF f key acc
    where
        f Nothing =
            case kind of
                TableKind -> Right (Just (FrameTable Defined Map.empty))
                ArrayTableKind -> Right (Just (FrameArray (Map.empty :| [])))
        f (Just (FrameTable Implicit t)) =
            case kind of
                TableKind -> Right (Just (FrameTable Defined t))
                ArrayTableKind -> Left "attempt to redefine table as array table"
        f (Just (FrameTable Defined _)) =
            Left "attempt to redefine table"
        f (Just (FrameValue {})) = Left "attempt to defined table over value"
        f (Just (FrameArray a)) =
            case kind of
                TableKind -> Left "attempt to open array table as table"
                ArrayTableKind -> Right (Just (FrameArray (NonEmpty.cons Map.empty a)))

addTop (key :| k1:keys) kind acc = Map.alterF f key acc
    where
        keys' = k1 :| keys
        f Nothing = Just . FrameTable Implicit <$> addTop keys' kind acc
        f (Just (FrameTable k t)) = Just . FrameTable k <$> addTop keys' kind t
        f (Just (FrameValue{})) = Left "attempt to traverse value as table"
        f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> addTop keys' kind t

assign :: [String] -> Key -> Value -> Map String Frame -> Either String (Map String Frame)

assign (p:prefix) key val acc = Map.alterF f p acc
    where
        f Nothing = error "underdefined prefix indicates logic error"

        -- when a dotted key mentions an existing table, it just gets traversed
        f (Just (FrameTable k t)) = Just . FrameTable k <$> assign prefix key val t

        -- when an existing frame array is mentioned, traverse the last element
        f (Just (FrameArray (t:|ts))) = Just . FrameArray . (:|ts) <$> assign prefix key val t

        f (Just (FrameValue{})) = Left "attempted to traverse a primitive value"

assign [] (key :| []) val acc = Map.alterF f key acc
    where
        f Nothing = Right (Just (FrameValue val))
        f Just{} = Left "key already assigned"

assign [] (key:| k1:keys) val acc = Map.alterF f key acc
    where
        keys' = k1:|keys
        -- when a dotted key introduces a table, that defines it
        f Nothing = Just . FrameTable Defined <$> assign [] keys' val Map.empty

        -- ??? does this define an implicit table ???
        f (Just (FrameTable _ t)) = Just . FrameTable Defined <$> assign [] keys' val t
        
        f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> assign [] keys' val t

        f (Just (FrameValue{})) = Left "attempted to traverse a primitive value"