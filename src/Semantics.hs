module Semantics (compileExprs) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Pretty (prettyKey)
import Raw (Key, Val(..), Expr(..))
import Value (Value(..))
import Control.Monad (foldM)

data SectionKind = TableKind | ArrayTableKind
    deriving Show

type KeyVals = [(Key, Val)]

gather :: [Expr] -> (KeyVals, [(SectionKind, Key, KeyVals)])
gather = goTop []
    where
        goTop acc []                           = (reverse acc, [])
        goTop acc (ArrayTableExpr key : exprs) = (reverse acc, goTable ArrayTableKind key [] exprs)
        goTop acc (TableExpr      key : exprs) = (reverse acc, goTable TableKind      key [] exprs)
        goTop acc (KeyValExpr k v     : exprs) = goTop ((k,v):acc) exprs

        goTable kind key acc []                         = (kind, key, reverse acc) : []
        goTable kind key acc (TableExpr      k : exprs) = (kind, key, reverse acc) : goTable TableKind k [] exprs
        goTable kind key acc (ArrayTableExpr k : exprs) = (kind, key, reverse acc) : goTable ArrayTableKind k [] exprs
        goTable kind key acc (KeyValExpr k v   : exprs) = goTable kind key ((k,v):acc) exprs

compileExprs :: [Expr] -> Either String (Map String Value)
compileExprs exprs =
 do let (topKVs, tables) = gather exprs
    m1 <- assignKeyVals Map.empty topKVs
    m2 <- foldM (\m (kind, key, kvs) -> addTop key kind m \m_ -> assignKeyVals m_ kvs) m1 tables
    pure (fmap frameToValue m2)

assignKeyVals :: Map String Frame -> KeyVals -> Either String (Map String Frame)
assignKeyVals =
    foldM \m (k,v) ->
     do value <- valToValue v
        assign k value m

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

addTop :: Key -> SectionKind -> Map String Frame -> (Map String Frame -> Either String (Map String Frame)) -> Either String (Map String Frame)

addTop (key :| []) kind acc k = Map.alterF f key acc
    where
        -- defining a new table
        f Nothing =
            case kind of
                TableKind      -> Just . FrameTable Defined   <$> k Map.empty
                ArrayTableKind -> Just . FrameArray . (:| []) <$> k Map.empty

        -- defining a super table of a previously defined subtable
        f (Just (FrameTable Implicit t)) =
            case kind of
                TableKind      -> Just . FrameTable Defined <$> k t
                ArrayTableKind -> Left "attempt to redefine table as array table"

        -- Add a new array element to an existing table array
        f (Just (FrameArray a)) =
            case kind of
                ArrayTableKind -> Just . FrameArray . (`NonEmpty.cons` a) <$> k Map.empty
                TableKind      -> Left "attempt to open array table as table"

        -- failure cases
        f (Just (FrameTable Defined _)) = Left "attempt to redefine table"
        f (Just (FrameValue {}))        = Left "attempt to redefined a value"

addTop (key :| k1:keys) kind acc k = Map.alterF f key acc
    where
        keys' = k1 :| keys
        f Nothing                       = Just . FrameTable Implicit  <$> addTop keys' kind Map.empty k
        f (Just (FrameTable tk t))      = Just . FrameTable tk        <$> addTop keys' kind t         k
        f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> addTop keys' kind t         k
        f (Just (FrameValue{}))         = Left "attempt to redefine a value"

assign :: Key -> Value -> Map String Frame -> Either String (Map String Frame)

assign (key :| []) val acc = Map.alterF f key acc
    where
        f Nothing = Right (Just (FrameValue val))
        f Just{}  = Left "key already assigned"

assign (key:| k1:keys) val acc = Map.alterF f key acc
    where
        keys' = k1:|keys
        -- when a dotted key introduces a table, that defines it
        f Nothing = Just . FrameTable Defined <$> assign keys' val Map.empty

        -- ??? does this define an implicit table ???
        f (Just (FrameTable _ t)) = Just . FrameTable Defined <$> assign keys' val t

        f (Just (FrameArray (t :| ts))) = Just . FrameArray . (:| ts) <$> assign keys' val t

        f (Just (FrameValue{})) = Left "attempted to traverse a primitive value"