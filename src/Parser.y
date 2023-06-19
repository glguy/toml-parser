{
{-|
Module      : Parser
Description : Raw TOML expression parser
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module parses TOML tokens into a list of raw,
uninterpreted sections and assignments.

-}
module Parser (parseRawToml) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Located (Located(Located, locPosition))
import Position (posLine)
import Raw
import Token

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
BAREKEY         { Located _ (TokBareKey        $$)  }
STRING          { Located _ (TokString         $$)  }
MLSTRING        { Located _ (TokMlString       $$)  }
INTEGER         { Located _ (TokInteger        $$)  }
FLOAT           { Located _ (TokFloat          $$)  }
COMMENT         { Located _ (TokComment        $$)  }
OFFSETDATETIME  { Located _ (TokOffsetDateTime $$)  }
LOCALDATETIME   { Located _ (TokLocalDateTime  $$)  }
LOCALDATE       { Located _ (TokLocalDate      $$)  }
LOCALTIME       { Located _ (TokLocalTime      $$)  }
EOF             { Located _ TokEOF                  }

%monad          { Either (Located Token)            }
%error          { errorP                            }

%name parseRawToml toml

%%

toml ::                       { [Expr]        }
  : sepBy1(line, NEWLINE) EOF { concat $1     }

line ::                   { [Expr]              }
  :            commentok  { []                  }
  | expression commentok  { [$1]                }

expression ::                 { Expr                            }
  : getLineNo keyval          { KeyValExpr $1 (fst $2) (snd $2) }
  | getLineNo '['  key ']'    { TableExpr      $1 $3            }
  | getLineNo '[[' key ']]'   { ArrayTableExpr $1 $3            }

getLineNo ::        { Int }
  :                 {%^ Right . posLine . locPosition }

keyval ::           { (Key, Val)              }
  : key '=' val     { ($1,$3)                 }

key ::              { Key                     }
  : sepBy1(simplekey, '.') { $1 }

simplekey ::        { String                  }
  : BAREKEY         { $1                      }
  | STRING          { $1                      }

val ::              { Val                     }
  : INTEGER         { ValInteger    $1        }
  | FLOAT           { ValFloat      $1        }
  | 'true'          { ValBool       True      }
  | 'false'         { ValBool       False     }
  | STRING          { ValString     $1        }
  | MLSTRING        { ValString     $1        }
  | LOCALDATE       { ValDay        $1        }
  | LOCALTIME       { ValTimeOfDay  $1        }
  | OFFSETDATETIME  { ValZonedTime  $1        }
  | LOCALDATETIME   { ValLocalTime  $1        }
  | array           { ValArray      $1        }
  | inlinetable     { ValTable      $1        }

inlinetable ::                  { [(Key, Val)]      }
  : '{' sepBy(keyval, ',') '}'  { $2                }

array ::                                                  { [Val]       }
  : '[' commentnewline                                ']' { []          }
  | '[' commentnewline arrayvalues                    ']' { reverse $3  }
  | '[' commentnewline arrayvalues ',' commentnewline ']' { reverse $3  }

arrayvalues ::                                        { [Val]       }
  :                                val commentnewline { [$1]        }
  | arrayvalues ',' commentnewline val commentnewline { $4 : $1     }

commentnewline ::                    {}
  :                                  {}
  | commentnewline commentok NEWLINE {}

commentok :: {}
  :          {}
  | COMMENT  {}

sepBy(p,q) ::         { [p]                   }
  :                   { []                    }
  | sepBy1(p,q)       { NonEmpty.toList $1    }

sepBy1(p,q) ::        { NonEmpty p            }
  : sepBy1_(p,q)      { NonEmpty.reverse $1   }

sepBy1_(p,q) ::       { NonEmpty p            }
  : p                 { NonEmpty.singleton $1 }
  | sepBy1_(p,q) q p  { NonEmpty.cons $3 $1   }

{

errorP :: [Located Token] -> Either (Located Token) a
errorP (t:_) = Left t
errorP []    = error "Parser.errorP: unterminated token stream"

}
