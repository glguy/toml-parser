{-# LANGUAGE RankNTypes #-}
{-|
Module      : Toml.FromValue.Matcher
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
module Toml.FromValue.Matcher (
    -- * Types
    Matcher,
    Result(..),
    MatchMessage(..),

    -- * Operations
    runMatcher,
    withScope,
    getScope,
    warning,

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

-- | Computations that result in a 'Result' and which track a list
-- of nested contexts to assist in generating warnings and error
-- messages.
--
-- Use 'withScope' to run a 'Matcher' in a new, nested scope.
newtype Matcher a = Matcher {
    unMatcher ::
        forall r.
        [Scope] ->
        DList MatchMessage ->
        (DList MatchMessage -> r) ->
        (DList MatchMessage -> a -> r) ->
        r
    }

instance Functor Matcher where
    fmap = liftM

instance Applicative Matcher where
    pure x = Matcher (\_env warn _err ok -> ok warn x)
    (<*>) = ap

instance Monad Matcher where
    m >>= f = Matcher (\env warn err ok -> unMatcher m env warn err (\warn' x -> unMatcher (f x) env warn' err ok))
    {-# INLINE (>>=) #-}

instance Alternative Matcher where
    empty = Matcher (\_env _warn err _ok -> err mempty)
    Matcher x <|> Matcher y = Matcher (\env warn err ok -> x env warn (\errs1 -> y env warn (\errs2 -> err (errs1 <> errs2)) ok) ok)

instance MonadPlus Matcher

-- | Scopes for TOML message.
--
-- @since 1.3.0.0
data Scope
    = ScopeIndex Int -- ^ zero-based array index
    | ScopeKey String -- ^ key in a table
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
--
-- @since 1.3.0.0
data MatchMessage = MatchMessage {
    matchPath :: [Scope], -- ^ path to message location
    matchMessage :: String -- ^ error and warning message body
    } deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

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
--
-- @since 1.3.0.0
data Result e a
    = Failure [e]   -- ^ error messages
    | Success [e] a -- ^ warning messages and result
    deriving (
        Read {- ^ Default instance -},
        Show {- ^ Default instance -},
        Eq   {- ^ Default instance -},
        Ord  {- ^ Default instance -})

-- | Run a 'Matcher' with an empty scope.
--
-- @since 1.3.0.0
runMatcher :: Matcher a -> Result MatchMessage a
runMatcher (Matcher m) = m [] mempty (Failure . runDList) (Success . runDList)

-- | Run 'Matcher' and ignore warnings.
--
-- @since 1.3.3.0
runMatcherIgnoreWarn :: Matcher a -> Either [MatchMessage] a
runMatcherIgnoreWarn m =
    case runMatcher m of
        Failure err -> Left err
        Success _ x -> Right x

-- | Run 'Matcher' and treat warnings as errors.
--
-- @since 1.3.3.0
runMatcherFatalWarn :: Matcher a -> Either [MatchMessage] a
runMatcherFatalWarn m =
    case runMatcher m of
        Success [] x   -> Right x
        Success warn _ -> Left warn
        Failure err    -> Left err

-- | Run a 'Matcher' with a locally extended scope.
--
-- @since 1.3.0.0
withScope :: Scope -> Matcher a -> Matcher a
withScope ctx (Matcher m) = Matcher (\env -> m (ctx : env))

-- | Get the current list of scopes.
--
-- @since 1.3.0.0
getScope :: Matcher [Scope]
getScope = Matcher (\env warn _err ok -> ok warn (reverse env))

-- | Emit a warning mentioning the current scope.
warning :: String -> Matcher ()
warning w =
 do loc <- getScope
    Matcher (\_env warn _err ok -> ok (warn <> one (MatchMessage loc w)) ())

-- | Fail with an error message annotated to the current location.
instance MonadFail Matcher where
    fail e =
     do loc <- getScope
        Matcher (\_env _warn err _ok -> err (one (MatchMessage loc e)))

-- | Update the scope with the message corresponding to a table key
--
-- @since 1.3.0.0
inKey :: String -> Matcher a -> Matcher a
inKey = withScope . ScopeKey

-- | Update the scope with the message corresponding to an array index
--
-- @since 1.3.0.0
inIndex :: Int -> Matcher a -> Matcher a
inIndex = withScope . ScopeIndex
