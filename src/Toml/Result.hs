{-|
Module      : Toml.Result
Description : Computation result tracking warnings and failure
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This type functions like a more elaborate @'Either' 'String'@ extended
to also track warning messages, in addition to error messages.

Emit error messages using 'fail'.

Emit warning messages using 'warn'.

-}
module Toml.Result (
    Result(..),
    warn,

    -- * Lists for warnings
    List,
    fromList,
    ) where

import Control.Monad (ap, MonadPlus (mzero, mplus), liftM)
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Foldable (Foldable(toList))

-- | Computations that can fail with an error message or succeed with zero
-- or more warning messages.
data Result a
    = Failure String
    | Success !(List String) a -- warnings and result
    deriving (Show, Eq, Ord)

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

-- | A list type that deals with appends well on the left and right side
data List a
    = Nil
    | One a
    | App (List a) (List a)

instance Eq a => Eq (List a) where
    x == y = toList x == toList y

instance Ord a => Ord (List a) where
    compare x y = compare (toList x) (toList y)

instance Show a => Show (List a) where
    showsPrec p x = showParen (p >= 11) (showString "fromList " . shows (toList x))

instance Monoid (List a) where
    mempty = Nil

instance Semigroup (List a) where
    Nil <> t = t
    t <> Nil = t
    a <> b = App a b

instance Foldable List where
    foldr _ z Nil       = z
    foldr f z (One a  ) = f a z
    foldr f z (App a b) = foldr f (foldr f z b) a

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = go x xs
    where
        go y []     = One y
        go y (z:zs) = App (One y) (go z zs)
