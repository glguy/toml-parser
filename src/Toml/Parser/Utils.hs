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
    asMlString,
    asBareKey,
    asInteger,
    asBool,
    asFloat,
    asOffsetDateTime,
    asLocalDate,
    asLocalTime,
    asLocalDateTime,
    locVal,

    lexerP,
    errorP,

    -- * Lexer-state management
    push,
    pop,
    ) where

import Data.Text (Text)
import Data.Time
import Toml.Lexer (scanToken, Context(..))
import Toml.Lexer.Token (Token(..))
import Toml.Located (Located(..))
import Toml.Pretty (prettyToken)
import Toml.Position (Position)

-- continuation passing implementation of a state monad with errors
newtype Parser r a = P {
    getP ::
        [Context] -> Located Text ->
        ([Context] -> Located Text -> a -> Either (Located String) r) ->
        Either (Located String) r
    }

-- | Run the top-level parser
runParser :: Parser r r -> Context -> Located Text -> Either (Located String) r
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
errorP e = P \_ _ _ -> Left (fmap (\t -> "parse error: unexpected " ++ prettyToken t) e)

-- | Operation the parser generator uses to request the next token.
lexerP :: (Located Token -> Parser r a) -> Parser r a
lexerP f = P \st str k ->
    case scanToken (head st) str of
        Left le -> Left (("lexical error: " ++) <$> le)
        Right (t, str') -> getP (f t) st str' k
{-# Inline lexerP #-}


asString :: Token -> Maybe Text
asString =
    \case
        TokString i -> Just i
        _ -> Nothing

asBareKey :: Token -> Maybe Text
asBareKey =
    \case
        TokBareKey i -> Just i
        _ -> Nothing

asMlString :: Token -> Maybe Text
asMlString =
    \case
        TokMlString i -> Just i
        _ -> Nothing


asInteger :: Token -> Maybe Integer
asInteger =
    \case
        TokInteger i -> Just i
        _ -> Nothing

asBool :: Token -> Maybe Bool
asBool =
    \case
        TokTrue -> Just True
        TokFalse -> Just False
        _ -> Nothing

asFloat :: Token -> Maybe Double
asFloat =
    \case
        TokFloat x -> Just x
        _ -> Nothing

asOffsetDateTime :: Token -> Maybe ZonedTime
asOffsetDateTime =
    \case
        TokOffsetDateTime x -> Just x
        _ -> Nothing


asLocalDateTime :: Token -> Maybe LocalTime
asLocalDateTime =
    \case
        TokLocalDateTime x -> Just x
        _ -> Nothing


asLocalDate :: Token -> Maybe Day
asLocalDate =
    \case
        TokLocalDate x -> Just x
        _ -> Nothing

asLocalTime :: Token -> Maybe TimeOfDay
asLocalTime =
    \case
        TokLocalTime x -> Just x
        _ -> Nothing

locVal :: (Position -> a -> b) -> Located a -> b
locVal f (Located l x) = f l x
