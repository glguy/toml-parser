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

    Result(..),
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (asks, local, ReaderT(..))
import Control.Monad.Trans.Writer.CPS (runWriterT, tell, WriterT)
import Data.Monoid (Endo(..))

-- | Computations that result in a 'Result' and which track a list
-- of nested contexts to assist in generating warnings and error
-- messages.
--
-- Use 'withScope' to run a 'Matcher' in a new, nested scope.
newtype Matcher a = Matcher (ReaderT [String] (WriterT (DList String) (Either String)) a)
    deriving (Functor, Applicative, Monad)

type DList a = Endo [a]

-- | Computation outcome with error and warning messages.
data Result a
    = Failure String -- error message
    | Success [String] a -- warnings and result
    deriving (Read, Show, Eq, Ord)

-- | Run a 'Matcher' with an empty scope.
runMatcher :: Matcher a -> Result a
runMatcher (Matcher m) =
    case runWriterT (runReaderT m []) of
        Left e -> Failure e
        Right (x,w) -> Success (w `appEndo` []) x

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
    Matcher (lift (tell (Endo ((w ++ " in top" ++ concat loc):))))

-- | Fail with an error message annotated to the current location.
instance MonadFail Matcher where
    fail e =
     do loc <- getScope
        Matcher (lift (lift (Left (e ++ " in top" ++ concat loc))))
