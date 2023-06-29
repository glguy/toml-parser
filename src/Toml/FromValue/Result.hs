{-|
Module      : Toml.FromValue.Result
Description : Computation result tracking warnings and failure
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This type functions like a more elaborate @'Either' 'String'@ extended
to also track warning messages, in addition to error messages.

Emit error messages using 'fail'.

Emit warning messages using 'warn'.

-}
module Toml.FromValue.Result (
    Result(..),
    warn,

    -- * Lists for warnings
    List,
    fromList,
    ) where

import Control.Monad (ap, MonadPlus (mzero, mplus), liftM)
import Control.Applicative (Alternative (empty, (<|>)))
import Toml.FromValue.List

-- | Computations that can fail with an error message or succeed with zero
-- or more warning messages.
data Result a
    = Failure String
    | Success !(List String) a -- warnings and result
    deriving (Show, Eq, Ord)

-- | Emit a single warning message and succeed returning @()@.
warn :: String -> Result ()
warn w = Success (One w) ()

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure = Success Nil
    (<*>) = ap

instance Monad Result where
    Failure e >>= _ = Failure e
    Success w1 x >>= f =
        case f x of
            Success w2 y -> Success (w1 <> w2) y
            Failure e    -> Failure e

-- | Left-biased alternatives. First success and first error are preferred.
instance Alternative Result where
    empty = Failure "Alternative:mzero"
    Failure _ <|> Success ws x = Success ws x
    e         <|> _            = e

instance MonadPlus Result where
    mzero = empty
    mplus = (<|>)

-- | @'fail' = 'Failure'@
instance MonadFail Result where
    fail = Failure
