{
{-# OPTIONS_GHC -Wnot #-}
{-# LANGUAGE Trustworthy #-}
{-|
Module      : Lexer
Description : Lexer for TOML
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Lexer where

import LexerUtils
import Tokens
import Data.Text (Text)
import qualified Data.Text as Text

}

$asciialpha     = [A-Z a-z]
$digit          = [0-9]
$octdigit       = [0-7]
$hexdigit       = [0-9a-fA-F]

@decimal        = $digit+

@barekey        = ($asciialpha | $digit | \_ | \-)+

@newline        = \r? \n

@fractpart      = $digit+ (\_ $digit+)*
@integer        = [\-\+]? (0 | [1-9] $digit* (\_ $digit+)*)
@double         = @integer (\. @fractpart)? ([eE] @integer)?


@day            = @decimal \- $digit{2} \- $digit{2}
@timeofday      = @decimal \: @decimal \: @decimal (\. @decimal)?
@localtime      = @day T @timeofday
@zonedtime      = @localtime ( [a-zA-Z] | [\+\-] $digit{2} \:? $digit{2} )


toml :-

<0> {
$white+                 ;
"#" .*                  ;

"{"                     { token_ LeftBrace              }
"}"                     { token_ RightBrace             }
"["                     { token_ LeftBracket            }
"]"                     { token_ RightBracket           }
","                     { token_ Comma                  }
"."                     { token_ Period                 }
"="                     { token_ EqualSign              }
@integer                { token integer                 }
@double                 { token double                  }
"true"                  { token_ TrueToken              }
"false"                 { token_ FalseToken             }
@localtime              { token localtime               }
@zonedtime              { token zonedtime               }
@timeofday              { token timeofday               }
@day                    { token day                     }
@barekey                { token BareKey                 }

'''      @newline ?     { startString mlsq              }
\" \" \" @newline ?     { startString mldq              }
'                       { startString slsq              }
\"                      { startString sldq              }
}

<mlsq> '''              { endString                     }
<mldq> \" \" \"         { endString                     }
<slsq> '                { endString                     }
<sldq> \"               { endString                     }

<mlsq,mldq> @newline    { emitChar                      }
<sldq,mldq> {
\\ b                    { emitChar' '\b'                }
\\ t                    { emitChar' '\t'                }
\\ n                    { emitChar' '\n'                }
\\ f                    { emitChar' '\f'                }
\\ r                    { emitChar' '\r'                }
\\ \"                   { emitChar' '"'                 }
\\ \\                   { emitChar' '\\'                }
\\ u $hexdigit{4}       { emitShortUnicode              }
\\ U $hexdigit{8}       { emitLongUnicode               }
\\ @newline $white *;
\\                      { token_ (Error BadEscape)      }
}

<sldq,slsq,mldq,mlsq> . { emitChar                      }

{
-- | Attempt to produce a token stream from an input file.
-- In the case of an error the line and column of the error
-- are returned instead.
scanTokens ::
  Text            {- ^ Source text          -} ->
  [Located Token] {- ^ Tokens with position -}
scanTokens str = go (Located startPos str) InNormal
  where
  go inp st =
    case alexScan inp (stateToInt st) of
      AlexEOF                -> eofAction (locPosition inp) st
      AlexError inp'         -> errorAction inp'
      AlexSkip  inp' _       -> go inp' st
      AlexToken inp' len act -> case act len inp st of
                                  (st', xs) -> xs ++ go inp' st'

-- | Compute the Alex state corresponding to a particular 'LexerMode'
stateToInt :: LexerMode -> Int
stateToInt InNormal{}           = 0
stateToInt (InString mode _ _)  = mode

}
