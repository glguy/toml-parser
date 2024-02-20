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
module Toml.Lexer (Context(..), scanToken, lexValue, Token(..)) where

import Data.Text (Text)
import Data.Text qualified as Text
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
$control          = [\x00-\x1F \x7F]

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

@bad_dec_int = [\-\+]? 0 ($digit | _ $digit)+

$non_eol = [\x09 \x20-\x7E $non_ascii]
@comment = $comment_start_symbol $non_eol*

$literal_char = [\x09 \x20-\x26 \x28-\x7E $non_ascii]

$mll_char = [\x09 \x20-\x26 \x28-\x7E]
@mll_content = $mll_char | @newline

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

@bad_dec_int        { failure "leading zero prohibited" }
@dec_int            { token mkDecInteger                }
@hex_int            { token mkHexInteger                }
@oct_int            { token mkOctInteger                }
@bin_int            { token mkBinInteger                }
@float              { token mkFloat                     }
"true"              { token_ TokTrue                    }
"false"             { token_ TokFalse                   }

@offset_date_time   { timeValue "offset date-time" offsetDateTimePatterns TokOffsetDateTime }
@local_date         { timeValue "local date"       localDatePatterns      TokLocalDate      }
@local_date_time    { timeValue "local date-time"  localDateTimePatterns  TokLocalDateTime  }
@local_time         { timeValue "local time"       localTimePatterns      TokLocalTime      }

}

<0> {
"[["                { token_ Tok2SquareO                }
"]]"                { token_ Tok2SquareC                }
}

<0,val,tab> {
@newline            { token_ TokNewline                 }
@comment;
$wschar+;

"="                 { token_ TokEquals                  }
"."                 { token_ TokPeriod                  }
","                 { token_ TokComma                   }

"["                 { token_ TokSquareO                 }
"]"                 { token_ TokSquareC                 }
"{"                 { token_ TokCurlyO                  }
"}"                 { token_ TokCurlyC                  }

@barekey            { textToken TokBareKey              }

\"{3} @newline?     { startMlBstr                       }
\"                  { startBstr                         }
"'''" @newline?     { startMlLstr                       }
"'"                 { startLstr                         }

}

<lstr> {
  $literal_char+    { strFrag                           }
  "'"               { endStr . fmap (Text.drop 1)            }
}

<bstr> {
  $unescaped+       { strFrag                           }
  \"                { endStr . fmap (Text.drop 1)            }
}

<mllstr> {
  @mll_content+     { strFrag                           }
  "'" {1,2}         { strFrag                           }
  "'" {3,5}         { endStr . fmap (Text.drop 3)            }
}

<mlbstr> {
  @mlb_escaped_nl;
  ($unescaped | @newline)+ { strFrag                    }
  \" {1,2}          { strFrag                           }
  \" {3,5}          { endStr . fmap (Text.drop 3)            }
}

<mlbstr, bstr> {
  \\ U $hexdig{8}   { unicodeEscape                     }
  \\ U              { failure "\\U requires exactly 8 hex digits"}
  \\ u $hexdig{4}   { unicodeEscape                     }
  \\ u              { failure "\\u requires exactly 4 hex digits"}
  \\ n              { strFrag . (Text.singleton '\n' <$)               }
  \\ t              { strFrag . (Text.singleton '\t' <$)               }
  \\ r              { strFrag . (Text.singleton '\r' <$)               }
  \\ f              { strFrag . (Text.singleton '\f' <$)               }
  \\ b              { strFrag . (Text.singleton '\b' <$)               }
  \\ \\             { strFrag . (Text.singleton '\\' <$)               }
  \\ \"             { strFrag . (Text.singleton '\"' <$)               }
  $control # [\t\r\n] { recommendEscape                 }
}

{

type AlexInput = Located Text

alexGetByte :: AlexInput -> Maybe (Int, AlexInput)
alexGetByte = locatedUncons

-- | Get the next token from a located string. This function can be total
-- because one of the possible token outputs is an error token.
scanToken :: Context -> Located Text -> Either (Located String) (Located Token, Located Text)
scanToken st str =
  case alexScan str (stateInt st) of
    AlexEOF          -> eofToken st str
    AlexError str'   -> Left (mkError . Text.unpack <$> str')
    AlexSkip  str' _ -> scanToken st str'
    AlexToken str' n action ->
      case action (Text.take n <$> str) st of
        Resume st'   -> scanToken st' str'
        LexerError e -> Left e
        EmitToken  t -> Right (t, str')

stateInt :: Context -> Int
stateInt TopContext      = 0
stateInt TableContext    = tab
stateInt ValueContext    = val
stateInt BstrContext  {} = bstr
stateInt MlBstrContext{} = mlbstr
stateInt LstrContext  {} = lstr
stateInt MlLstrContext{} = mllstr

-- | Lex a single token in a value context. This is mostly useful for testing.
lexValue :: Text -> Either String Token
lexValue str =
    case scanToken ValueContext Located{ locPosition = startPos, locThing = str } of
      Left e -> Left (locThing e)
      Right (t,_) -> Right (locThing t)

}
