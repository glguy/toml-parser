{-|
Module      : QuoteStr
Description : Quasiquoter for multi-line string literals
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module makes it easy to write inline TOML for
test cases without worrying about escaping newlines
or quotation marks.

-}
module QuoteStr (quoteStr) where

import Language.Haskell.TH ( Exp(LitE), ExpQ, Lit(StringL) )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Data.List ( stripPrefix )

quoteStr :: QuasiQuoter
quoteStr = QuasiQuoter {
    quoteDec = \_ -> fail "quoteStr doesn't support declarations",
    quotePat = \_ -> fail "quoteStr doesn't support patterns",
    quoteType = \_ -> fail "quoteStr doesn't support types",
    quoteExp = processString
  }

processString :: String -> ExpQ
processString ('\n':xs) =
    let ws = takeWhile (' '==) xs
        
        cleanup "" = pure ""
        cleanup x = case stripPrefix ws x of
                      Nothing -> fail "bad prefix"
                      Just x' -> pure x'
    in LitE . StringL . unlines <$> traverse cleanup (lines xs)
processString _ = fail "malformed string literal"