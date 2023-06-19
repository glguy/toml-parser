{-|
Module      : Toml.Position
Description : File position representation
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the 'Position' type for tracking locations
in files while doing lexing and parsing for providing more useful
error messages.

-}
module Toml.Position where

-- | A position in a text file
data Position = Position
  { posIndex, posLine, posColumn :: {-# UNPACK #-} !Int }
  deriving (Read, Show, Ord, Eq)

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
