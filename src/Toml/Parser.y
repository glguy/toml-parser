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
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Text.Printf (printf)

import Toml.Lexer (Context(..), Token(..), scanToken)
import Toml.Located (Located(Located, locThing))
import Toml.Parser.Types
import Toml.Position (Position(..), startPos)
import Toml.Pretty (prettyToken)

}

%tokentype      { Located Token                     }
%token
'true'          { Located _ TokTrue                 }
'false'         { Located _ TokFalse                }
','             { Located _ TokComma                }
'='             { Located _ TokEquals               }
NEWLINE         { Located _ TokNewline              }
'.'             { Located _ TokPeriod               }
'['             { Located _ TokSquareO              }
']'             { Located _ TokSquareC              }
'[['            { Located _ Tok2SquareO             }
']]'            { Located _ Tok2SquareC             }
'{'             { Located _ TokCurlyO               }
'}'             { Located _ TokCurlyC               }
BAREKEY         { Located _ (TokBareKey        _ )  }
STRING          { Located _ (TokString         _ )  }
MLSTRING        { Located _ (TokMlString       $$)  }
INTEGER         { Located _ (TokInteger        $$)  }
FLOAT           { Located _ (TokFloat          $$)  }
OFFSETDATETIME  { Located _ (TokOffsetDateTime $$)  }
LOCALDATETIME   { Located _ (TokLocalDateTime  $$)  }
LOCALDATE       { Located _ (TokLocalDate      $$)  }
LOCALTIME       { Located _ (TokLocalTime      $$)  }

%monad          { M }
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

keyval ::           { (Key, Val)              }
  : key rhs '=' pop val { ($1,$5)             }

key ::              { Key                     }
  : sepBy1(simplekey, '.') { $1               }

simplekey ::        { Located String          }
  : BAREKEY         { fmap asString $1        }
  | STRING          { fmap asString $1        }

val ::              { Val                     }
  : INTEGER         { ValInteger    $1        }
  | FLOAT           { ValFloat      $1        }
  | 'true'          { ValBool       True      }
  | 'false'         { ValBool       False     }
  | STRING          { ValString (asString (locThing $1)) }
  | MLSTRING        { ValString     $1        }
  | LOCALDATE       { ValDay        $1        }
  | LOCALTIME       { ValTimeOfDay  $1        }
  | OFFSETDATETIME  { ValZonedTime  $1        }
  | LOCALDATETIME   { ValLocalTime  $1        }
  | array           { ValArray      $1        }
  | inlinetable     { ValTable      $1        }

inlinetable ::      { [(Key, Val)]            }
  : lhs '{' sepBy(keyval, ',') pop '}'
                    { $3                      }

array ::            { [Val]                   }
  : rhs '[' newlines                          pop ']' { []          }
  | rhs '[' newlines arrayvalues              pop ']' { reverse $4  }
  | rhs '[' newlines arrayvalues ',' newlines pop ']' { reverse $4  }

arrayvalues ::      { [Val]                   }
  :                          val newlines { [$1]    }
  | arrayvalues ',' newlines val newlines { $4 : $1 }

newlines ::         {                         }
  :                 {                         }
  | newlines NEWLINE{                         }

sepBy(p,q) ::       { [p]                     }
  :                 { []                      }
  | sepBy1(p,q)     { NonEmpty.toList $1      }

sepBy1(p,q) ::      { NonEmpty p              }
  : sepBy1_(p,q)    { NonEmpty.reverse $1     }

sepBy1_(p,q) ::     { NonEmpty p              }
  :                p{ pure $1                 }
  | sepBy1_(p,q) q p{ NonEmpty.cons $3 $1     }

rhs ::              {                         }
  :                 {% push ValueContext      }

lhs ::              {                         }
  :                 {% push NameContext       }

pop ::              {                         }
  :                 {% pop                    }

{

type M = StateT ([Context], Located String) (Either (Located String))

-- | Parse a list of tokens either returning the first unexpected
-- token or a list of the TOML statements in the file to be
-- processed by "Toml.Semantics".
parseRawToml :: String -> Either (Located String) [Expr]
parseRawToml str = evalStateT parseRawToml_ ([NameContext], Located startPos str)

-- | Add a new context to the lexer context stack
push :: Context -> M ()
push x = modify \(st, str) -> (x : st, str)

-- | Pop the top context off the lexer context stack
pop :: M ()
pop = modify \(_ : st, str) -> (st, str)

-- | Operation the parser generator uses when it reaches an unexpected token.
errorP :: Located Token -> M a
errorP = failure . fmap \t -> "parse error: unexpected " ++ prettyToken t

-- | Abort with a located error message.
failure :: Located String -> M a
failure = lift . Left

-- | Operation the parser generator uses to request the next token.
lexerP :: (Located Token -> M a) -> M a
lexerP k =
 do (st, str) <- get
    case scanToken (head st) str of
      Left le -> failure (("lexical error: " ++) <$> le)
      Right (t, str') -> put (st, str') >> k t

-- | Extract the string content of a bare-key or a quoted string.
asString :: Token -> String
asString (TokString x) = x
asString (TokBareKey x) = x
asString _ = error "simpleKeyLexeme: panic"

}
