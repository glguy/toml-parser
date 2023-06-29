{-|
Module      : Toml.FromValue.Matcher
Description : A type for building results while tracking scopes
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Toml.FromValue.Matcher ( 
    Matcher,
    runMatcher,
    withScope,
    getScope,
    warning,
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks, local, ReaderT(..))
import Toml.FromValue.Result (Result, warn)

-- | Computations that result in a 'Result' and which track a list
-- of nested contexts to assist in generating warnings and error
-- messages.
--
-- Use 'withScope' to run a 'Matcher' in a new, nested scope.
newtype Matcher a = Matcher (ReaderT [String] Result a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

-- | Run a 'Matcher' with an empty scope.
runMatcher :: Matcher a -> Result a
runMatcher (Matcher m) = runReaderT m []

-- | Run a 'Matcher' with a locally extended scope.
withScope :: String -> Matcher a -> Matcher a
withScope ctx (Matcher m) = Matcher (local (ctx:) m)

-- | Get the current list of scopes.
getScope :: Matcher [String]
getScope = Matcher (asks reverse)

-- | Emit a warning mentioning the current scope.
warning :: String -> Matcher ()
warning w =
 do loc <- getScope
    Matcher (lift (warn (w ++ " in top" ++ concat loc)))

-- | Fail with an error message annotated to the current location.
instance MonadFail Matcher where
    fail e =
     do loc <- getScope
        Matcher (fail (e ++ " in " ++ concat loc))
