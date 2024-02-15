{-|
Module      : Toml.FromValue.ParseTable
Description : A type for matching keys out of a table
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides utilities for matching key-value pairs
out of tables while building up application-specific values.

It will help generate warnings for unused keys, help select
between multiple possible keys, and emit location-specific
error messages when keys are unavailable.

This module provides the 'ParseTable' implementation, but
most of the basic functionality is exported directly from
"Toml.FromValue".

-}
module Toml.FromValue.ParseTable (
    -- * Base interface
    ParseTable,
    KeyAlt(..),
    pickKey,
    runParseTable,

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
import Data.Map qualified as Map
import Toml.FromValue.Matcher (warning, Matcher, inKey, failAt, warningAt)
import Toml.Pretty (prettySimpleKey)
import Toml.Value (Table'(..), Value')

-- | A 'Matcher' that tracks a current set of unmatched key-value
-- pairs from a table.
--
-- Use 'Toml.FromValue.optKey' and 'Toml.FromValue.reqKey' to extract keys.
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

-- | Run a 'ParseTable' computation with a given starting 'Table'.
-- Unused tables will generate a warning. To change this behavior
-- 'getTable' and 'setTable' can be used to discard or generate
-- error messages.
runParseTable :: ParseTable l a -> l -> Table' l -> Matcher l a
runParseTable (ParseTable p) l t =
 do (x, MkTable t') <- runStateT (runReaderT p l) t
    for_ (Map.assocs t') \(k, (a, _)) ->
        warningAt a ("unexpected key: " ++ show (prettySimpleKey k))
    pure x


-- | Return the remaining portion of the table being matched.
getTable :: ParseTable l (Table' l)
getTable = ParseTable (lift get)

-- | Replace the remaining portion of the table being matched.
setTable :: Table' l -> ParseTable l ()
setTable = ParseTable . lift . put

-- | Emit a warning at the current location.
warnTable :: String -> ParseTable l ()
warnTable = liftMatcher . warning

warnTableAt :: l -> String -> ParseTable l ()
warnTableAt l = liftMatcher . warningAt l

failTableAt :: l -> String -> ParseTable l a
failTableAt l = liftMatcher . failAt l

-- | Key and value matching function
--
-- @since 1.2.0.0
data KeyAlt l a
    = Key String (Value' l -> Matcher l a) -- ^ pick alternative based on key match
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
-- 'Toml.FromValue.reqKey' cases together with @('<|>')@ because that will
-- generate one error message for each unmatched alternative as well as
-- the error associate with the matched alternative.
--
-- @since 1.2.0.0
pickKey :: [KeyAlt l a] -> ParseTable l a
pickKey xs =
 do MkTable t <- getTable
    foldr (f t) errCase xs
    where
        f _ (Else m) _ = liftMatcher m
        f t (Key k c) continue =
            case Map.lookup k t of
                Nothing -> continue
                Just (_, v) ->
                 do setTable $! MkTable (Map.delete k t)
                    liftMatcher (inKey k (c v))

        errCase =
         do l <- ParseTable ask
            case xs of
                []        -> empty -- there's nothing a user can do here
                [Key k _] -> failTableAt l ("missing key: " ++ show (prettySimpleKey k))
                _         -> failTableAt l ("possible keys: " ++ intercalate ", " [show (prettySimpleKey k) | Key k _ <- xs])
