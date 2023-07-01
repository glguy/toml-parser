{
{-|
Module      : Toml.Lexer
Description : TOML lexical analyzer
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module parses a TOML file into a lazy sequence
of tokens. The lexer is aware of nested brackets and
equals signs in order to handle TOML's context-sensitive
lexing requirements. This context enables the lexer to
distinguish between bare keys and various values like:
floating-point literals, integer literals, and date literals.

This module uses actions and lexical hooks defined in
"LexerUtils".

-}
module Toml.Lexer (scanTokens, lexValue, Token(..)) where

import Control.Monad.Trans.State.Strict (evalState, runState)
import Toml.Lexer.Token
import Toml.Lexer.Utils
import Toml.Located
import Toml.Position

}
$non_ascii        = \x1
$wschar           = [\ \t]

@ws               = $wschar*
@newline          = \r? \n

$bindig           = [0-1]
$octdig           = [0-7]
$digit            = [0-9]
$hexdig           = [ $digit A-F a-f ]
$basic_unescaped  = [ $wschar \x21 \x23-\x5B \x5D-\x7E $non_ascii ]
$comment_start_symbol = \#

@barekey = [0-9 A-Z a-z \- _]+

@unsigned_dec_int = $digit | [1-9] ($digit | _ $digit)+
@dec_int = [\-\+]? @unsigned_dec_int
@zero_prefixable_int = $digit ($digit | _ $digit)*
@hex_int = "0x" $hexdig ($hexdig | _ $hexdig)*
@oct_int = "0o" $octdig ($octdig | _ $octdig)*
@bin_int = "0b" $bindig ($bindig | _ $bindig)*

@frac = "." @zero_prefixable_int
@float_exp_part = [\+\-]? @zero_prefixable_int
@special_float = [\+\-]? ("inf" | "nan")
@exp = [Ee] @float_exp_part
@float_int_part = @dec_int
@float = @float_int_part ( @exp | @frac @exp? ) | @special_float

$non_eol = [\x09 \x20-\x7E $non_ascii]
@comment = $comment_start_symbol $non_eol*

$literal_char = [\x09 \x20-\x26 \x28-\x7E $non_ascii]
@literal_string = "'" $literal_char* "'"

@ml_literal_string_delim = "'''"
$mll_char = [\x09 \x20-\x26 \x28-\x7E]
@mll_quotes = "'" "'"?
@mll_content = $mll_char | @newline
@ml_literal_body = @mll_content* (@mll_quotes @mll_content+)* @mll_quotes?
@ml_literal_string = @ml_literal_string_delim @newline? @ml_literal_body @ml_literal_string_delim

@mlb_escaped_nl = \\ @ws @newline ($wschar | @newline)*
$unescaped = [$wschar \x21 \x23-\x5B \x5D-\x7E $non_ascii]

@date_fullyear  = $digit {4}
@date_month     = $digit {2}
@date_mday      = $digit {2}
$time_delim     = [Tt\ ]
@time_hour      = $digit {2}
@time_minute    = $digit {2}
@time_second    = $digit {2}
@time_secfrac   = "." $digit+
@time_numoffset = [\+\-] @time_hour ":" @time_minute
@time_offset    = [Zz] | @time_numoffset

@partial_time = @time_hour ":" @time_minute ":" @time_second @time_secfrac?
@full_date = @date_fullyear "-" @date_month "-" @date_mday
@full_time = @partial_time @time_offset

@offset_date_time = @full_date $time_delim @full_time
@local_date_time = @full_date $time_delim @partial_time
@local_date = @full_date
@local_time = @partial_time

toml :-

<val> {

@dec_int            { value mkDecInteger                }
@hex_int            { value mkHexInteger                }
@oct_int            { value mkOctInteger                }
@bin_int            { value mkBinInteger                }
@float              { value mkFloat                     }
"true"              { value_ TokTrue                    }
"false"             { value_ TokFalse                   }

@offset_date_time   { timeValue "offset date-time" offsetDateTimePatterns TokOffsetDateTime }
@local_date         { timeValue "local date"       localDatePatterns      TokLocalDate      }
@local_date_time    { timeValue "local date-time"  localDateTimePatterns  TokLocalDateTime  }
@local_time         { timeValue "local time"       localTimePatterns      TokLocalTime      }

}

<0> {
"[["                { token_ Tok2SquareO                }
"]]"                { token_ Tok2SquareC                }
}

<0,val> {
@newline            { token_ TokNewline                 }
@comment;
$wschar+;

@literal_string     { value mkLiteralString             }

@ml_literal_string  { value mkMlLiteralString           }

"="                 { equals                            }
"."                 { token_ TokPeriod                  }
","                 { token_ TokComma                   }

"["                 { squareO                           }
"]"                 { squareC                           }
"{"                 { curlyO                            }
"}"                 { curlyC                            }

@barekey            { token TokBareKey                  }

\"{3} @newline?     { startMlStr                        }
\"                  { startStr                          }

}

<str> {
  $unescaped+       { strFrag                           }
  \"                { endStr . fmap (drop 1)            }
}

<mlstr> {
  @mlb_escaped_nl;
  ($unescaped | @newline)+ { strFrag                    }
  \" {1,2}          { strFrag                           }
  \" {3,5}          { endStr . fmap (drop 3)            }
}

<mlstr, str> {
  \\ U $hexdig{8}   { unicodeEscape                     }
  \\ u $hexdig{4}   { unicodeEscape                     }
  \\ n              { strFrag . ("\n" <$)               }
  \\ t              { strFrag . ("\t" <$)               }
  \\ r              { strFrag . ("\r" <$)               }
  \\ f              { strFrag . ("\f" <$)               }
  \\ b              { strFrag . ("\b" <$)               }
  \\ \\             { strFrag . ("\\" <$)               }
  \\ \"             { strFrag . ("\"" <$)               }
}

{

-- | Generate a lazy-list of tokens from the input string.
-- The token stream is guaranteed to be terminated either with
-- 'TokEOF' or 'TokError'.
scanTokens :: String -> [Located Token]
scanTokens str = scanTokens' [] Located { locPosition = startPos, locThing = str }

scanTokens' :: [Context] -> AlexInput -> [Located Token]
scanTokens' st str =
  case alexScan str (stateInt st) of
    AlexEOF          -> [eofToken st str]
    AlexError str'   -> [mkError <$> str']
    AlexSkip  str' _ -> scanTokens' st str'
    AlexToken str' n action ->
      case runState (action (take n <$> str)) st of
        (t, st') -> t ++ scanTokens' st' str'

eofToken :: [Context] -> Located String -> Located Token
eofToken (MlStrContext p _ : _) _ = Located p (TokError "unterminated multi-line string literal")
eofToken (StrContext   p _ : _) _ = Located p (TokError "unterminated string literal")
eofToken (ListContext  p   : _) _ = Located p (TokError "unterminated '['")
eofToken (TableContext p   : _) _ = Located p (TokError "unterminated '{'")
eofToken _                       s = TokEOF <$ s

stateInt :: [Context] -> Int
stateInt (ValueContext   : _) = val
stateInt (ListContext {} : _) = val
stateInt (StrContext  {} : _) = str
stateInt (MlStrContext{} : _) = mlstr
stateInt _                    = 0

-- | Lex a single token in a value context. This is mostly useful for testing.
lexValue :: String -> Token
lexValue str = lexValue_ Located { locPosition = startPos, locThing = str }

lexValue_ :: Located String -> Token
lexValue_ str =
  case alexScan str val of
    AlexEOF              -> TokError "end of input"
    AlexError{}          -> TokError "lexer error"
    AlexSkip str' _      -> lexValue_ str'
    AlexToken _ n action ->
      case evalState (action (take n <$> str)) [ValueContext] of
        t:_ -> locThing t
        []  -> TokError "lexer error"

}
