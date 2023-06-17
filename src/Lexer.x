{
module Lexer (scanTokens) where

import Data.Char (isSpace)
import Data.Functor ((<&>))
import Control.Monad.Trans.State (runState)

import LexerUtils
import Located
import Position
import Token

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

@escape_seq_char  = [\x22 \x5C \x62 \x66 \x6E \x72 \x74] | "u" $hexdig{4} | "U" $hexdig{8}
@escaped          = \\ @escape_seq_char
@basic_char       = $basic_unescaped | @escaped

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

$non_eol = [\x09 \x20-\x7F $non_ascii]
@comment = $comment_start_symbol $non_eol*

$literal_char = [ \x09 \x20-\x26 \x28-\x7E $non_ascii ]
@basic_string = \" @basic_char* \"
@literal_string = "'" $literal_char* "'"

@ml_literal_string_delim = "'''"
$mll_char = [\x09 \x20-\x26 \x28-\x7E]
@mll_quotes = "'" "'"?
@mll_content = $mll_char | @newline
@ml_literal_body = @mll_content* (@mll_quotes @mll_content+)* @mll_quotes?
@ml_literal_string = @ml_literal_string_delim @newline? @ml_literal_body @ml_literal_string_delim

@ml_basic_string_delim = \" \" \"
@mlb_quotes = \" \"?
@mlb_escaped_nl = \\ @ws @newline ($wschar | @newline)*
$mlb_unescaped = [$wschar \x21 \x23-\x5B \x5D-\x7E $non_ascii]
@mlb_char = $mlb_unescaped | @escaped
@mlb_content = @mlb_char | @newline | @mlb_escaped_nl
@ml_basic_body = @mlb_content* (@mlb_quotes @mlb_content+)* @mlb_quotes?
@ml_basic_string = @ml_basic_string_delim @newline? @ml_basic_body @ml_basic_string_delim

@date_fullyear = $digit {4}
@date_month = $digit {2}
@date_mday = $digit {2}
$time_delim = [T\ ]
@time_hour = $digit {2}
@time_minute = $digit {2}
@time_second = $digit {2}
@time_secfrac = "." $digit+
@time_numoffset = [\+\-] @time_hour ":" @time_minute
@time_offset = "Z" | @time_numoffset

@partial_time = @time_hour ":" @time_minute ":" @time_second @time_secfrac?
@full_date = @date_fullyear "-" @date_month "-" @date_mday
@full_time = @partial_time @time_offset

@offset_date_time = @full_date $time_delim @full_time
@local_date_time = @full_date $time_delim @partial_time
@local_date = @full_date
@local_time = @partial_time

toml :-

$wschar+;
@comment        { token TokComment                      }
@newline        { token_ TokNewline                     }
@basic_string   { value mkBasicString                   }
@literal_string { value mkLiteralString                 }

"}"             { exitTable                             }
"]"             { exitList                              }
"="             { equals                                }
"."             { token_ TokPeriod                      }
","             { token_ TokComma                       } 

<0> {

"["             { token_ TokSquareO                     }
"]"             { token_ TokSquareC                     }
"{"             { token_ TokCurlyO                      }
"[["            { token_ Tok2SquareO                    }
"]]"            { token_ Tok2SquareC                    }
@barekey        { token  TokBareKey                     }

}

<val> {

"["                 { enterList                         }
"{"                 { enterTable                        }
@dec_int            { value mkDecInteger                }
@hex_int            { value mkHexInteger                }
@oct_int            { value mkOctInteger                }
@bin_int            { value mkBinInteger                }
@float              { value mkFloat                     }
"true"              { value_ TokTrue                    }
"false"             { value_ TokFalse                   }
@ml_literal_string  { value mkMlLiteralString           }
@ml_basic_string    { value mkMlBasicString             }

@offset_date_time   { timeValue "offset date-time" offsetDateTimePatterns TokOffsetDateTime }
@local_date         { timeValue "local date"       localDatePatterns      TokLocalDate }
@local_date_time    { timeValue "local date-time"  localDateTimePatterns  TokLocalDateTime }
@local_time         { timeValue "local time"       localTimePatterns      TokLocalTime }

}

{
scanTokens :: String -> [Located Token]
scanTokens str = scanTokens' [] Located { locPosition = startPos, locThing = str }

scanTokens' :: [Context] -> AlexInput -> [Located Token]
scanTokens' st str =
  case alexScan str (stateInt st) of
    AlexEOF -> [TokEOF <$ str]
    AlexError str' -> [str' <&> \txt -> TokError ("Bad lexeme: " ++ show (takeWhile (not . isSpace) txt))]
    AlexSkip  str' _ -> scanTokens' st str'
    AlexToken str' n action ->
      case runState (traverse action (take n <$> str)) st of
        (t, st') -> t : scanTokens' st' str'

stateInt :: [Context] -> Int
stateInt (ValueContext : _) = val
stateInt (ListContext  : _) = val
stateInt _                  = 0

}
