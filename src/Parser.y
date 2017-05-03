{
{-# LANGUAGE Trustworthy #-}

module Parser (parseComponents) where

import Tokens
import Data.Text (Text,pack)
import Components
import Value

}

%tokentype                      { Located Token                 }
%token
STRING                          { Located _ (String $$)         }
BAREKEY                         { Located _ (BareKey $$)        }
INTEGER                         { Located _ (Integer $$)        }
DOUBLE                          { Located _ (Double $$)         }
'true'                          { Located _ TrueToken           }
'false'                         { Located _ FalseToken          }
'['                             { Located _ LeftBracket         }
']'                             { Located _ RightBracket        }
'{'                             { Located _ LeftBrace           }
'}'                             { Located _ RightBrace          }
','                             { Located _ Comma               }
'.'                             { Located _ Period              }
'='                             { Located _ EqualSign           }
ZONEDTIME                       { Located _ (ZonedTimeTok $$)   }
LOCALTIME                       { Located _ (LocalTimeTok $$)   }
TIMEOFDAY                       { Located _ (TimeOfDayTok $$)   }
DAY                             { Located _ (DayTok       $$)   }
EOF                             { Located _ EOF                 }

%monad { Either (Located Token) }
%error { errorP }

%name components

%%

components ::                   { [Component]                   }
  : componentsR EOF             { reverse $1                    }

componentsR ::                  { [Component]                   }
  :                             { []                            }
  | componentsR component       { $2 : $1                       }

component ::                    { Component                     }
  : '['     keys     ']'        { TableEntry $2                 }
  | '[' '[' keys ']' ']'        { ArrayEntry $3                 }
  | key '=' value               { KeyValue $1 $3                }

keys ::                         { [Text]                        }
  : keysR                       { reverse $1                    }

keysR ::                        { [Text]                        }
  : key                         { [$1]                          }
  | keysR '.' key               { $3 : $1                       }

key ::                          { Text                          }
  : BAREKEY                     { $1                            }
  | STRING                      { $1                            }
  | INTEGER                     { pack (show $1)                }
  | 'true'                      { pack "true"                   }
  | 'false'                     { pack "false"                  }

value ::                        { Value                         }
  : INTEGER                     { IntegerV  $1                  }
  | DOUBLE                      { DoubleV   $1                  }
  | STRING                      { StringV   $1                  }
  | ZONEDTIME                   { ZonedTimeV $1                 }
  | TIMEOFDAY                   { TimeOfDayV $1                 }
  | DAY                         { DayV       $1                 }
  | LOCALTIME                   { LocalTimeV $1                 }
  | 'true'                      { BoolV True                    }
  | 'false'                     { BoolV False                   }
  | '{' inlinetable '}'         { TableV    $2                  }
  | '[' inlinearray ']'         { ListV     $2                  }

inlinetable ::                  { [(Text,Value)]                }
  :                             { []                            }
  | inlinetableR                { reverse $1                    }

inlinetableR ::                 { [(Text,Value)]                }
  : key '=' value               { [($1,$3)]                     }
  | inlinetableR ',' key '=' value
                                { ($3,$5):$1                    }

inlinearray ::                  { [Value]                       }
  :                             { []                            }
  | inlinearrayR                { reverse $1                    }
  | inlinearrayR ','            { reverse $1                    }

inlinearrayR ::                 { [Value]                       }
  : value                       { [$1]                          }
  | inlinearrayR ',' value      { $3 : $1                       }

{

errorP :: [Located Token] -> Either (Located Token) a
errorP xs = Left (head xs)

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseComponents ::
  [Located Token]                    {- ^ layout annotated token stream -} ->
  Either (Located Token) [Component] {- ^ token at failure or result -}
parseComponents = components

}
