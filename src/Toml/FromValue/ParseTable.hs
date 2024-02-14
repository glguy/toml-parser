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
    setTable,
    getTable,
    ) where

import Control.Applicative (Alternative, empty)
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), get, put)
import Data.List (intercalate)
import Data.Map qualified as Map
import Toml.FromValue.Matcher (warning, Matcher, inKey)
import Toml.Located (Located)
import Toml.Pretty (prettySimpleKey)
import Toml.Value (Table', Value')

-- | A 'Matcher' that tracks a current set of unmatched key-value
-- pairs from a table.
--
-- Use 'Toml.FromValue.optKey' and 'Toml.FromValue.reqKey' to extract keys.
--
-- Use 'getTable' and 'setTable' to override the table and implement
-- other primitives.
newtype ParseTable a = ParseTable (StateT Table' Matcher a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

-- | Implemented in terms of 'fail' on 'Matcher'
instance MonadFail ParseTable where
    fail = ParseTable . fail

-- | Lift a matcher into the current table parsing context.
liftMatcher :: Matcher a -> ParseTable a
liftMatcher = ParseTable . lift

-- | Run a 'ParseTable' computation with a given starting 'Table'.
-- Unused tables will generate a warning. To change this behavior
-- 'getTable' and 'setTable' can be used to discard or generate
-- error messages.
runParseTable :: ParseTable a -> Table' -> Matcher a
runParseTable (ParseTable p) t =
 do (x, t') <- runStateT p t
    case Map.keys t' of
        []  -> pure x
        [k] -> x <$ warning ("unexpected key: " ++ show (prettySimpleKey k))
        ks  -> x <$ warning ("unexpected keys: " ++ intercalate ", " (map (show . prettySimpleKey) ks))

-- | Return the remaining portion of the table being matched.
getTable :: ParseTable Table'
getTable = ParseTable get

-- | Replace the remaining portion of the table being matched.
setTable :: Table' -> ParseTable ()
setTable = ParseTable . put

-- | Emit a warning at the current location.
warnTable :: String -> ParseTable ()
warnTable = ParseTable . lift . warning


-- | Key and value matching function
--
-- @since 1.2.0.0
data KeyAlt a
    = Key String (Located Value' -> Matcher a) -- ^ pick alternative based on key match
    | Else (Matcher a) -- ^ default case when no previous cases matched

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
pickKey :: [KeyAlt a] -> ParseTable a
pickKey xs =
 do t <- getTable
    foldr (f t) errCase xs
    where
        f _ (Else m) _ = liftMatcher m
        f t (Key k c) continue =
            case Map.lookup k t of
                Nothing -> continue
                Just (_, v) ->
                 do setTable $! Map.delete k t
                    liftMatcher (inKey k (c v))

        errCase =
            case xs of
                []        -> empty -- there's nothing a user can do here
                [Key k _] -> fail ("missing key: " ++ show (prettySimpleKey k))
                _         -> fail ("possible keys: " ++ intercalate ", " [show (prettySimpleKey k) | Key k _ <- xs])
