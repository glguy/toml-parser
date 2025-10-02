{-|
Module      : Toml.Schema.ParseTable
Description : A type for matching keys out of a table
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides utilities for matching key-value pairs
out of tables while building up application-specific values.

It will help generate warnings for unused keys, help select
between multiple possible keys, and emit location-specific
error messages when keys are unavailable.

-}
module Toml.Schema.ParseTable (
    -- * Base interface
    ParseTable,
    KeyAlt(..),
    pickKey,
    parseTable,

    -- * Primitives
    liftMatcher,
    warnTable,
    warnTableAt,
    failTableAt,
    setTable,
    getTable,
    ) where

import Control.Applicative (Alternative, empty)
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.State.Strict (StateT(..), get, put)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Toml.Schema.Matcher (Matcher, inKey, failAt, warn, warnAt)
import Toml.Semantics (Table'(..), Value')
import Toml.Pretty

-- | Parser that tracks a current set of unmatched key-value
-- pairs from a table.
--
-- Use 'Toml.Schema.optKey' and 'Toml.Schema.reqKey' to extract keys.
--
-- Use 'getTable' and 'setTable' to override the table and implement
-- other primitives.
newtype ParseTable l a = ParseTable (ReaderT l (StateT (Table' l) (Matcher l)) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

-- | Implemented in terms of 'fail' on 'Matcher'
instance MonadFail (ParseTable l) where
    fail = ParseTable . fail

-- | Lift a matcher into the current table parsing context.
liftMatcher :: Matcher l a -> ParseTable l a
liftMatcher = ParseTable . lift . lift

-- | Run a 'ParseTable' computation with a given starting 'Table''.
-- Unused tables will generate a warning. To change this behavior
-- 'getTable' and 'setTable' can be used to discard or generate
-- error messages.
parseTable :: ParseTable l a -> l -> Table' l -> Matcher l a
parseTable (ParseTable p) l t =
 do (x, MkTable t') <- runStateT (runReaderT p l) t
    for_ (OMap.assocs t') \(k, (a, _)) ->
        warnAt a ("unexpected key: " ++ show (prettySimpleKey k))
    pure x

-- | Return the remaining portion of the table being matched.
getTable :: ParseTable l (Table' l)
getTable = ParseTable (lift get)

-- | Replace the remaining portion of the table being matched.
setTable :: Table' l -> ParseTable l ()
setTable = ParseTable . lift . put

-- | Emit a warning without an annotation.
warnTable :: String -> ParseTable l ()
warnTable = liftMatcher . warn

-- | Emit a warning with the given annotation.
warnTableAt :: l -> String -> ParseTable l ()
warnTableAt l = liftMatcher . warnAt l

-- | Abort the current table matching with an error message at the given annotation.
failTableAt :: l -> String -> ParseTable l a
failTableAt l = liftMatcher . failAt l

-- | Key and value matching function
data KeyAlt l a
    = Key Text (Value' l -> Matcher l a) -- ^ pick alternative based on key match
    | Else (Matcher l a) -- ^ default case when no previous cases matched

-- | Take the first option from a list of table keys and matcher functions.
-- This operation will commit to the first table key that matches. If the
-- associated matcher fails, only that error will be propagated and the
-- other alternatives will not be matched.
--
-- If no keys match, an error message is generated explaining which keys
-- would have been accepted.
--
-- This is provided as an alternative to chaining multiple
-- 'Toml.Schema.reqKey' cases together with 'Control.Applicative.Alternative'
-- which will fall-through as a result of any failure to the next case.
pickKey :: [KeyAlt l a] -> ParseTable l a
pickKey xs =
 do MkTable t <- getTable
    foldr (f t) errCase xs
    where
        f _ (Else m) _ = liftMatcher m
        f t (Key k c) continue =
            case OMap.lookup k t of
                Nothing -> continue
                Just (_, v) ->
                 do setTable $! MkTable (OMap.delete k t)
                    liftMatcher (inKey k (c v))

        errCase =
         do l <- ParseTable ask
            case xs of
                []        -> empty -- there's nothing a user can do here
                [Key k _] -> failTableAt l ("missing key: " ++ show (prettySimpleKey k))
                _         -> failTableAt l ("possible keys: " ++ intercalate ", " [show (prettySimpleKey k) | Key k _ <- xs])
