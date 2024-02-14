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

toml ::                         { [Expr]    }
  : sepBy1(expression, NEWLINE) { concat $1 }

expression ::       { [Expr]                  }
  :                 { []                      }
  | keyval          { [KeyValExpr (fst $1) (snd $1)] }
  | '['  key ']'    { [TableExpr      $2    ] }
  | '[[' key ']]'   { [ArrayTableExpr $2    ] }

keyval ::           { (Key, Located Val)      }
  : key rhs '=' pop val { ($1,$5)             }

key ::              { Key                     }
  : sepBy1(simplekey, '.') { $1               }

simplekey ::        { Located String          }
  : BAREKEY         { $1        }
  | STRING          { $1        }

val ::              { Located Val             }
  : INTEGER         { fmap ValInteger    $1 }
  | FLOAT           { fmap ValFloat      $1 }
  | BOOL            { fmap ValBool       $1 }
  | STRING          { fmap ValString     $1 }
  | MLSTRING        { fmap ValString     $1 }
  | LOCALDATE       { fmap ValDay        $1 }
  | LOCALTIME       { fmap ValTimeOfDay  $1 }
  | OFFSETDATETIME  { fmap ValZonedTime  $1 }
  | LOCALDATETIME   { fmap ValLocalTime  $1 }
  | array           { fmap ValArray      $1 }
  | inlinetable     { fmap ValTable      $1 }

inlinetable ::      { Located [(Key, Located Val)]    }
  : lhs '{' sepBy(keyval, ',') pop '}'
                    { Located $2 $3 }

array ::            { Located [Located Val]  }
  : rhs '[' newlines                          pop ']' { Located $2 [] }
  | rhs '[' newlines arrayvalues              pop ']' { Located $2 (reverse $4) }
  | rhs '[' newlines arrayvalues ',' newlines pop ']' { Located $2 (reverse $4)  }

arrayvalues ::      { [Located Val]                 }
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
parseRawToml :: String -> Either (Located String) [Expr]
parseRawToml = runParser parseRawToml_ TopContext . Located startPos

}
