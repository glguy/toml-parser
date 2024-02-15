{
{-|
Module      : Toml.Parser
Description : Raw TOML expression parser
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module parses TOML tokens into a list of raw,
uninterpreted sections and assignments.

-}
module Toml.Parser (
  -- * Types
  Expr(..),
  SectionKind(..),
  Val(..),
  Key,

  -- * Parser
  parseRawToml,
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

import Toml.Lexer (Context(..), Token(..))
import Toml.Located (Located(Located, locThing))
import Toml.Position (Position)
import Toml.Parser.Types (Expr(..), Key, Val(..), SectionKind(..))
import Toml.Parser.Utils
import Toml.Position (startPos)

}

%tokentype      { Located Token                     }
%token
','             { Located $$ TokComma                }
'='             { Located $$ TokEquals               }
NEWLINE         { Located $$ TokNewline              }
'.'             { Located $$ TokPeriod               }
'['             { Located $$ TokSquareO              }
']'             { Located $$ TokSquareC              }
'[['            { Located $$ Tok2SquareO             }
']]'            { Located $$ Tok2SquareC             }
'{'             { Located $$ TokCurlyO               }
'}'             { Located $$ TokCurlyC               }
BAREKEY         { (traverse asBareKey        -> Just $$) }
STRING          { (traverse asString         -> Just $$) }
MLSTRING        { (traverse asMlString       -> Just $$) }
BOOL            { (traverse asBool           -> Just $$) }
INTEGER         { (traverse asInteger        -> Just $$) }
FLOAT           { (traverse asFloat          -> Just $$) }
OFFSETDATETIME  { (traverse asOffsetDateTime -> Just $$) }
LOCALDATETIME   { (traverse asLocalDateTime  -> Just $$) }
LOCALDATE       { (traverse asLocalDate      -> Just $$) }
LOCALTIME       { (traverse asLocalTime      -> Just $$) }

%monad          { Parser r } { thenP } { pureP }
%lexer          { lexerP } { Located _ TokEOF }
%error          { errorP }

%name parseRawToml_ toml

%%

toml ::                         { [Expr Position] }
  : sepBy1(expression, NEWLINE) { concat $1 }

expression ::       { [Expr Position]         }
  :                 { []                      }
  | keyval          { [KeyValExpr (fst $1) (snd $1)] }
  | '['  key ']'    { [TableExpr      $2    ] }
  | '[[' key ']]'   { [ArrayTableExpr $2    ] }

keyval ::           { (Key Position, Val Position) }
  : key rhs '=' pop val { ($1,$5)             }

key ::              { Key Position            }
  : sepBy1(simplekey, '.') { $1               }

simplekey ::        { (Position, String)      }
  : BAREKEY         { locVal (,) $1           }
  | STRING          { locVal (,) $1           }

val ::              { Val Position            }
  : INTEGER         { locVal ValInteger    $1 }
  | FLOAT           { locVal ValFloat      $1 }
  | BOOL            { locVal ValBool       $1 }
  | STRING          { locVal ValString     $1 }
  | MLSTRING        { locVal ValString     $1 }
  | LOCALDATE       { locVal ValDay        $1 }
  | LOCALTIME       { locVal ValTimeOfDay  $1 }
  | OFFSETDATETIME  { locVal ValZonedTime  $1 }
  | LOCALDATETIME   { locVal ValLocalTime  $1 }
  | array           { locVal ValArray      $1 }
  | inlinetable     { locVal ValTable      $1 }

inlinetable ::      { Located [(Key Position, Val Position)] }
  : lhs '{' sepBy(keyval, ',') pop '}'
                    { Located $2 $3 }

array ::            { Located [Val Position]  }
  : rhs '[' newlines                          pop ']' { Located $2 [] }
  | rhs '[' newlines arrayvalues              pop ']' { Located $2 (reverse $4) }
  | rhs '[' newlines arrayvalues ',' newlines pop ']' { Located $2 (reverse $4)  }

arrayvalues ::      { [Val Position]                 }
  :                          val newlines { [$1]    }
  | arrayvalues ',' newlines val newlines { $4 : $1 }

newlines ::         { ()                      }
  :                 { ()                      }
  | newlines NEWLINE{ ()                      }

sepBy(p,q) ::       { [p]                     }
  :                 { []                      }
  | sepBy1(p,q)     { NonEmpty.toList $1      }

sepBy1(p,q) ::      { NonEmpty p              }
  : sepBy1_(p,q)    { NonEmpty.reverse $1     }

sepBy1_(p,q) ::     { NonEmpty p              }
  :                p{ pure $1                 }
  | sepBy1_(p,q) q p{ NonEmpty.cons $3 $1     }

rhs ::              { ()                      }
  :                 {% push ValueContext      }

lhs ::              { ()                      }
  :                 {% push TableContext      }

pop ::              { ()                      }
  :                 {% pop                    }

{

-- | Parse a list of tokens either returning the first unexpected
-- token or a list of the TOML statements in the file to be
-- processed by "Toml.Semantics".
parseRawToml :: String -> Either (Located String) [Expr Position]
parseRawToml = runParser parseRawToml_ TopContext . Located startPos

}
