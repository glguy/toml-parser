module Toml where

import Data.Map (Map)
import Lexer (scanTokens)
import Located (Located(locPosition, locThing))
import Position (prettyPosition)
import Parser (toml)
import Semantics (compileExprs)
import Token (prettyToken)
import Value (Value)

-- | Parse a TOML formatted 'String' or report an error message.
parse :: String -> Either String (Map String Value)
parse str =
    case toml (scanTokens str) of
        Left le ->
            Left ("Unexpected " ++ prettyToken (locThing le) ++ " at " ++ prettyPosition (locPosition le))
        Right exprs -> compileExprs exprs

