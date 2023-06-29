{-|
Module      : Toml.FromValue.List
Description : A list type for accumulating warning messages
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This type exists to efficiently append lists of warning
messages. There will often be no warning messages, so
this module handles appending empty elements efficiently
on both sides as well as avoid the quadratic blow-up
one would expect from repeated appending to the end.

-}
module Toml.FromValue.List where

import Data.Foldable (toList)

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

-- | Construct an abstract 'List' from a normal '[]'.
fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = go x xs
    where
        go y []     = One y
        go y (z:zs) = App (One y) (go z zs)
