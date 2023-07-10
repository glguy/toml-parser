{-|
Module      : Toml.Located
Description : Values annotated with positions
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a simple tuple for tracking pairs of
values and their file locations.

-}
module Toml.Located (
    Located(..)
    ) where

import Toml.Position (Position)

-- | A value annotated with its text file position
data Located a = Located
    { locPosition :: {-# UNPACK #-} !Position -- ^ position
    , locThing    :: !a -- ^ thing at position
    }
    deriving (
        Read        {- ^ Default instance -}, 
        Show        {- ^ Default instance -}, 
        Functor     {- ^ Default instance -}, 
        Foldable    {- ^ Default instance -}, 
        Traversable {- ^ Default instance -})
