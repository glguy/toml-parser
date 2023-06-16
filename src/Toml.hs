module Toml where

import Lexer ( scanTokens )
import qualified Parser 
import Token ( prettyToken )
import Located ( Located(locPosition, locThing) )
import Position ( prettyPosition )
import Semantics
import Value
import Data.Map (Map)

rawparse :: String -> Either String (Map String Value)
rawparse str =
    case Parser.toml (scanTokens str) of
        Left le ->
            Left ("Unexpected " ++ prettyToken (locThing le) ++ " at " ++ prettyPosition (locPosition le))
        Right exprs -> compileExprs exprs

