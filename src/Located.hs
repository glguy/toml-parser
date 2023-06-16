module Located where

import Position (Position)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: {-# UNPACK #-} !Position
  , locThing    :: !a
  }
  deriving (Read, Show, Functor, Foldable, Traversable)
