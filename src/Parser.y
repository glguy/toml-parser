{
{-# LANGUAGE Trustworthy #-}
{-|
Module      : Parser
Description : Parser for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Parser (parseComponents) where

import Tokens
import Data.Text (Text,pack)
import Components
import Value
import Errors

}

%tokentype                      { Located Token                 }
%token
STRING                          { Located _ (String $$)         }
BAREKEY                         { Located _ (BareKey $$)        }
INTEGER                         { Located _ (Integer $$)        }
DOUBLE                          { Located _ (Double $$)         }
'true'                          { Located _ TrueToken           }
'false'                         { Located _ FalseToken          }
'['                             { $$@(Located _ LeftBracket)    }
']'                             { Located _ RightBracket        }
'{'                             { $$@(Located _ LeftBrace)      }
'}'                             { Located _ RightBrace          }
','                             { Located _ Comma               }
'.'                             { Located _ Period              }
'='                             { Located _ EqualSign           }
ZONEDTIME                       { Located _ (ZonedTimeTok $$)   }
LOCALTIME                       { Located _ (LocalTimeTok $$)   }
TIMEOFDAY                       { Located _ (TimeOfDayTok $$)   }
DAY                             { Located _ (DayTok       $$)   }
EOF                             { Located _ EOF                 }

%monad { Either TOMLError }
%error { errorP }

%name components

%%

components ::                   { [Component]                   }
  : componentsR EOF             { reverse $1                    }

componentsR ::                  { [Component]                   }
  : keyvalues                   { [InitialEntry $1]             }
  | componentsR component keyvalues { $2 $3 : $1                }

component ::                    { [(Text,Value)] -> Component   }
  : '['     keys     ']'        { TableEntry $2                 }
  | '[' '[' keys ']' ']'        { ArrayEntry $3                 }

  | '['     keys     error      {% unterminated $1              }
  | '[' '[' keys     error      {% unterminated $2              }
  | '[' '[' keys ']' error      {% unterminated $1              }

keyvalues ::                    { [(Text,Value)]                }
  : keyvaluesR                  { reverse $1                    }

keyvaluesR ::                   { [(Text,Value)]                }
  :                             { []                            }
  | keyvaluesR key '=' value    { ($2,$4):$1                    }

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

  | '{' inlinetable error       {% unterminated $1              }
  | '[' inlinearray error       {% unterminated $1              }

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

errorP :: [Located Token] -> Either TOMLError a
errorP = Left . Unexpected . head

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseComponents ::
  [Located Token]              {- ^ layout annotated token stream -} ->
  Either TOMLError [Component] {- ^ token at failure or result -}
parseComponents = components

unterminated :: Located Token -> Either TOMLError a
unterminated = Left . Unterminated

}
