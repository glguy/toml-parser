{-# Language DeriveTraversable #-}
{-|
Module      : Located
Description : /Internal:/ Wrapper for tracking text-file location of things
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Located where

-- | A position in a text file
data Position = Position
  { posIndex  :: {-# UNPACK #-} !Int -- ^ zero-based character index
  , posLine   :: {-# UNPACK #-} !Int -- ^ one-based line number
  , posColumn :: {-# UNPACK #-} !Int -- ^ one-based column number
  }
  deriving (Read, Show)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: {-# UNPACK #-} !Position -- ^ position information
  , locThing    :: !a                       -- ^ annotated value
  }
  deriving (Read, Show, Functor, Foldable, Traversable)

-- | The initial 'Position' for the start of a file
startPos :: Position
startPos = Position { posIndex = 0, posLine = 1, posColumn = 1 }
