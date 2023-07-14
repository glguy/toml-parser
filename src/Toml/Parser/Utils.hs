{-|
Module      : Toml.Parser.Utils
Description : Primitive operations used by the happy-generated parser
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module contains all the primitives used by the Parser module.
By extracting it from the @.y@ file we minimize the amount of code
that has warnings disabled and get better editor support.

@since 1.3.0.0

-}
module Toml.Parser.Utils (
    Parser,
    runParser,
    pureP,
    thenP,
    asString,
    lexerP,
    errorP,

    -- * Lexer-state management
    push,
    pop,
    ) where

import Toml.Lexer (scanToken, Context(..))
import Toml.Lexer.Token (Token(TokBareKey, TokString))
import Toml.Located (Located)
import Toml.Parser.Types (Expr)
import Toml.Pretty (prettyToken)

-- continuation passing implementation of a state monad with errors
newtype Parser r a = P {
    getP ::
        [Context] -> Located String ->
        ([Context] -> Located String -> a -> Either (Located String) r) ->
        Either (Located String) r
    }

-- | Run the top-level parser
runParser :: Parser r r -> Context -> Located String -> Either (Located String) r
runParser (P k) ctx str = k [ctx] str \_ _ r -> Right r

-- | Bind implementation used in the happy-generated parser
thenP :: Parser r a -> (a -> Parser r b) -> Parser r b
thenP (P m) f = P \ctx str k -> m ctx str \ctx' str' x -> getP (f x) ctx' str' k
{-# Inline thenP #-}

-- | Return implementation used in the happy-generated parser
pureP :: a -> Parser r a
pureP x = P \ctx str k -> k ctx str x
{-# Inline pureP #-}

-- | Add a new context to the lexer context stack
push :: Context -> Parser r ()
push x = P \st str k -> k (x : st) str ()
{-# Inline push #-}

-- | Pop the top context off the lexer context stack. It is a program
-- error to pop without first pushing.
pop :: Parser r ()
pop = P \ctx str k ->
    case ctx of
        []       -> error "Toml.Parser.Utils.pop: PANIC! malformed production in parser"
        _ : ctx' -> k ctx' str ()
{-# Inline pop #-}

-- | Operation the parser generator uses when it reaches an unexpected token.
errorP :: Located Token -> Parser r a
errorP e = P \_ _ t -> Left (fmap (\t -> "parse error: unexpected " ++ prettyToken t) e)

-- | Operation the parser generator uses to request the next token.
lexerP :: (Located Token -> Parser r a) -> Parser r a
lexerP f = P \st str k ->
    case scanToken (head st) str of
        Left le -> Left (("lexical error: " ++) <$> le)
        Right (t, str') -> getP (f t) st str' k
{-# Inline lexerP #-}

-- | Extract the string content of a bare-key or a quoted string.
asString :: Token -> String
asString (TokString x) = x
asString (TokBareKey x) = x
asString _ = error "simpleKeyLexeme: panic"
{-# Inline asString #-}
