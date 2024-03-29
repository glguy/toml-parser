{-# LANGUAGE RankNTypes #-}
{-|
Module      : Toml.Schema.Matcher
Description : A type for building results while tracking scopes
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This type helps to build up computations that can validate a TOML
value and compute some application-specific representation.

It supports warning messages which can be used to deprecate old
configuration options and to detect unused table keys.

It supports tracking multiple error messages when you have more
than one decoding option and all of them have failed.

Use 'Toml.Pretty.prettyMatchMessage' for an easy way to make human
readable strings from matcher outputs.

-}
module Toml.Schema.Matcher (
    -- * Types
    Matcher,
    Result(..),
    MatchMessage(..),

    -- * Operations
    runMatcher,
    withScope,
    getScope,
    warn,
    warnAt,
    failAt,

    -- * Run helpers
    runMatcherIgnoreWarn,
    runMatcherFatalWarn,

    -- * Scope helpers
    Scope(..),
    inKey,
    inIndex,
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus, ap, liftM)
import Data.Monoid (Endo(..))
import Data.Text (Text)

-- | Computations that result in a 'Result' and which track a list
-- of nested contexts to assist in generating warnings and error
-- messages.
--
-- Use 'withScope' to run a 'Matcher' in a new, nested scope.
newtype Matcher l a = Matcher {
    unMatcher ::
        forall r.
        [Scope] ->
        DList (MatchMessage l) ->
        (DList (MatchMessage l) -> r) ->
        (DList (MatchMessage l) -> a -> r) ->
        r
    }

instance Functor (Matcher a) where
    fmap = liftM

instance Applicative (Matcher a) where
    pure x = Matcher (\_env ws _err ok -> ok ws x)
    (<*>) = ap

instance Monad (Matcher a) where
    m >>= f = Matcher (\env ws err ok -> unMatcher m env ws err (\warn' x -> unMatcher (f x) env warn' err ok))
    {-# INLINE (>>=) #-}

instance Alternative (Matcher a) where
    empty = Matcher (\_env _warn err _ok -> err mempty)
    Matcher x <|> Matcher y = Matcher (\env ws err ok -> x env ws (\errs1 -> y env ws (\errs2 -> err (errs1 <> errs2)) ok) ok)

instance MonadPlus (Matcher a)

-- | Scopes for TOML message.
data Scope
    = ScopeIndex Int -- ^ zero-based array index
    | ScopeKey Text -- ^ key in a table
    deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

-- | A message emitted while matching a TOML value. The message is paired
-- with the path to the value that was in focus when the message was
-- generated. These message get used for both warnings and errors.
--
-- For a convenient way to render these to a string, see 'Toml.Pretty.prettyMatchMessage'.
data MatchMessage a = MatchMessage {
    matchAnn :: Maybe a,
    matchPath :: [Scope], -- ^ path to message location
    matchMessage :: String -- ^ error and warning message body
    } deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -},
        Functor, Foldable, Traversable)

-- | List of strings that supports efficient left- and right-biased append
newtype DList a = DList (Endo [a])
    deriving (Semigroup, Monoid)

-- | Create a singleton list of strings
one :: a -> DList a
one x = DList (Endo (x:))

-- | Extract the list of strings
runDList :: DList a -> [a]
runDList (DList x) = x `appEndo` []

-- | Computation outcome with error and warning messages. Multiple error
-- messages can occur when multiple alternatives all fail. Resolving any
-- one of the error messages could allow the computation to succeed.
data Result e a
    = Failure [e]   -- ^ error messages
    | Success [e] a -- ^ warning messages and result
    deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

-- | Run a 'Matcher' with an empty scope.
runMatcher :: Matcher l a -> Result (MatchMessage l) a
runMatcher (Matcher m) = m [] mempty (Failure . runDList) (Success . runDList)

-- | Run 'Matcher' and ignore warnings.
runMatcherIgnoreWarn :: Matcher l a -> Either [MatchMessage l] a
runMatcherIgnoreWarn m =
    case runMatcher m of
        Failure err -> Left err
        Success _ x -> Right x

-- | Run 'Matcher' and treat warnings as errors.
runMatcherFatalWarn :: Matcher l a -> Either [MatchMessage l] a
runMatcherFatalWarn m =
    case runMatcher m of
        Success [] x   -> Right x
        Success ws _   -> Left ws
        Failure err    -> Left err

-- | Run a 'Matcher' with a locally extended scope.
withScope :: Scope -> Matcher l a -> Matcher l a
withScope scope (Matcher m) = Matcher (\scopes -> m (scope : scopes))

-- | Get the current list of scopes.
getScope :: Matcher a [Scope]
getScope = Matcher (\env ws _err ok -> ok ws (reverse env))

-- | Emit a warning without an annotation.
warn :: String -> Matcher a ()
warn w =
    Matcher (\scopes ws _err ok -> ok (ws <> one (MatchMessage Nothing (reverse scopes) w)) ())

-- | Emit a warning mentioning the given annotation.
warnAt :: l -> String -> Matcher l ()
warnAt loc w =
    Matcher (\scopes ws _err ok -> ok (ws <> one (MatchMessage (Just loc) (reverse scopes) w)) ())

-- | Fail with an error message without an annotation.
instance MonadFail (Matcher a) where
    fail e =
        Matcher (\scopes _warn err _ok -> err (one (MatchMessage Nothing (reverse scopes) e)))

-- | Terminate the match with an error mentioning the given annotation.
failAt :: l -> String -> Matcher l a
failAt l e =
    Matcher (\scopes _warn err _ok -> err (one (MatchMessage (Just l) (reverse scopes) e)))

-- | Update the scope with the message corresponding to a table key
inKey :: Text -> Matcher l a -> Matcher l a
inKey = withScope . ScopeKey

-- | Update the scope with the message corresponding to an array index
inIndex :: Int -> Matcher l a -> Matcher l a
inIndex = withScope . ScopeIndex
