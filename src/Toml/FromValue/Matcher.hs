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

    -- * Scope helpers
    Scope(..),
    inKey,
    inIndex,
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (asks, local, ReaderT(..))
import Control.Monad.Trans.Writer.CPS (runWriterT, tell, WriterT)
import Data.Monoid (Endo(..))

-- | Computations that result in a 'Result' and which track a list
-- of nested contexts to assist in generating warnings and error
-- messages.
--
-- Use 'withScope' to run a 'Matcher' in a new, nested scope.
newtype Matcher a = Matcher (ReaderT [Scope] (WriterT (DList MatchMessage) (Except (DList MatchMessage))) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

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
runMatcher (Matcher m) =
    case runExcept (runWriterT (runReaderT m [])) of
        Left e      -> Failure (runDList e)
        Right (x,w) -> Success (runDList w) x

-- | Run a 'Matcher' with a locally extended scope.
--
-- @since 1.3.0.0
withScope :: Scope -> Matcher a -> Matcher a
withScope ctx (Matcher m) = Matcher (local (ctx :) m)

-- | Get the current list of scopes.
--
-- @since 1.3.0.0
getScope :: Matcher [Scope]
getScope = Matcher (asks reverse)

-- | Emit a warning mentioning the current scope.
warning :: String -> Matcher ()
warning w =
 do loc <- getScope
    Matcher (lift (tell (one (MatchMessage loc w))))

-- | Fail with an error message annotated to the current location.
instance MonadFail Matcher where
    fail e =
     do loc <- getScope
        Matcher (lift (lift (throwE (one (MatchMessage loc e)))))

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
