{-|
Module      : TOML.ParserUtils
Description : /Internal:/ Parser support operations for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module is separate from the Parser.y input to Happy
to segregate the automatically generated code from the
hand written code. The automatically generated code
causes lots of warnings which mask the interesting warnings.
-}
module TOML.ParserUtils
  (
  -- * Errors
    errorP
  , unterminated
  ) where

import           TOML.Errors
import           TOML.Located
import           TOML.Tokens

-- | This operation is called by happy when no production matches the
-- current token list.
errorP :: [Located Token] {- ^ nonempty remainig tokens -} -> Either TOMLError a
errorP = Left . Unexpected . head

-- | Abort the parse with an error indicating that the given token was unmatched.
unterminated :: Located Token -> Either TOMLError a
unterminated = Left . Unterminated
