{
module Parser (toml) where

import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import Located (Located(Located))
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

%name toml

%%

toml ::                       { [Expr]        }
  : sepBy1(line, NEWLINE) EOF { concat $1     }

line ::                 { [Expr]              }
  :                     { []                  }
  |            COMMENT  { []                  }
  | expression          { [$1]                }
  | expression COMMENT  { [$1]                }

expression ::       { Expr                    }
  : keyval          { uncurry KeyValExpr $1   }
  | '['  key ']'    { TableExpr      $2       }
  | '[[' key ']]'   { ArrayTableExpr $2       }

keyval ::           { ([String], Val)         }
  : key '=' val     { ($1,$3)                 }

key ::              { [String]                }
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

array ::                                                  { [Val]       }
  : '[' commentnewline                                ']' { []          }
  | '[' commentnewline arrayvalues                    ']' { reverse $3  }
  | '[' commentnewline arrayvalues ',' commentnewline ']' { reverse $3  }

commentnewline ::                  { }
  : commentnewline COMMENT NEWLINE { }
  | commentnewline         NEWLINE { }
  |                                { }

arrayvalues ::                                        { [Val]       }
  :                                val commentnewline { [$1]        }
  | arrayvalues ',' commentnewline val commentnewline { $4 : $1     }

inlinetable ::                  { [([String], Val)] }
  : '{' sepBy(keyval, ',') '}'  { $2                }

sepBy(p,q) ::         { [p]                   }
  :                   { []                    }
  | sepBy1(p,q)       { $1                    }

sepBy1(p,q) ::        { [p]                   }
  : sepBy1_(p,q)      { reverse $1            }

sepBy1_(p,q) ::       { [p]                   }
  : p                 { [$1]                  }
  | sepBy1_(p,q) q p  { $3 : $1               }

{

errorP :: [Located Token] -> Either (Located Token) a
errorP (t:_) = Left t
errorP []    = error "Parser.errorP: unterminated token stream"

}
