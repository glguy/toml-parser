{-|
Module      : Toml.Syntax.Position
Description : File position representation
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the 'Position' type for tracking locations
in files while doing lexing and parsing for providing more useful
error messages.

This module assumes 8 column wide tab stops.

-}
module Toml.Syntax.Position (
    Located(..),
    Position(..),
    startPos,
    move,
    ) where

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

-- | A position in a text file
data Position = Position {
    posIndex  :: {-# UNPACK #-} !Int, -- ^ code-point index (zero-based)
    posLine   :: {-# UNPACK #-} !Int, -- ^ line index (one-based)
    posColumn :: {-# UNPACK #-} !Int  -- ^ column index (one-based)
    } deriving (
        Read    {- ^ Default instance -},
        Show    {- ^ Default instance -},
        Ord     {- ^ Default instance -},
        Eq      {- ^ Default instance -})

-- | The initial 'Position' for the start of a file
startPos :: Position
startPos = Position { posIndex = 0, posLine = 1, posColumn = 1 }

-- | Adjust a file position given a single character handling
-- newlines and tabs. All other characters are considered to fill
-- exactly one column.
move :: Char -> Position -> Position
move x Position{ posIndex = i, posLine = l, posColumn = c} =
    case x of
        '\n' -> Position{ posIndex = i+1, posLine = l+1, posColumn = 1 }
        '\t' -> Position{ posIndex = i+1, posLine = l, posColumn = (c + 7) `quot` 8 * 8 + 1 }
        _    -> Position{ posIndex = i+1, posLine = l, posColumn = c+1 }
