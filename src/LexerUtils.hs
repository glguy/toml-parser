module LexerUtils where

import Data.Char (isSpace, chr)
import Numeric

processDec :: String -> Integer
processDec ('+':xs) = read xs
processDec xs = read xs

processHex :: String -> Integer
processHex ('0':'x':xs) = fst (head (readHex xs))
processHex _ = error "processHex: bad input"

processOct :: String -> Integer
processOct ('0':'o':xs) = fst (head (readOct xs))
processOct _ = error "processHex: bad input"

processBin :: String -> Integer
processBin ('0':'b':xs) = fst (head (readBin xs))
processBin _ = error "processHex: bad input"

processFloat :: String -> Double
processFloat "nan" = 0/0
processFloat "+nan" = 0/0
processFloat "-nan" = 0/0
processFloat "inf" = 1/0
processFloat "+inf" = 1/0
processFloat "-inf" = -1/0
processFloat ('+':x) = read x
processFloat x = read x

processLiteral :: String -> String
processLiteral = tail . init

processBasic :: String -> String
processBasic "" = error "processBasic: missing initializer"
processBasic (_:start) = go start
  where
    go [] = error "processBasic: missing terminator"
    go "\"" = ""
    go ('\\':'"':xs) = '"' : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    go ('\\':'b':xs) = '\b' : go xs
    go ('\\':'f':xs) = '\f' : go xs
    go ('\\':'n':xs) = '\n' : go xs
    go ('\\':'r':xs) = '\r' : go xs
    go ('\\':'t':xs) = '\t' : go xs
    go ('\\':'u':a:b:c:d:xs) = chr (fst (head (readHex [a,b,c,d]))) : go xs
    go ('\\':'U':a:b:c:d:e:f:g:h:xs) = chr (fst (head (readHex [a,b,c,d,e,f,g,h]))) : go xs
    go (x:xs) = x : go xs

processMlBasic :: String -> String
processMlBasic str =
  case str of
    '"':'"':'"':'\r':'\n':start -> go start
    '"':'"':'"':'\n':start -> go start
    '"':'"':'"':start -> go start
    _ -> error "processMlBasic: missing initializer"
  where
    go "\"\"\"" = ""
    go ('\\':'"':xs) = '"' : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    go ('\\':'b':xs) = '\b' : go xs
    go ('\\':'f':xs) = '\f' : go xs
    go ('\\':'n':xs) = '\n' : go xs
    go ('\\':'r':xs) = '\r' : go xs
    go ('\\':'t':xs) = '\t' : go xs
    go ('\\':'u':a:b:c:d:xs) = chr (fst (head (readHex [a,b,c,d]))) : go xs
    go ('\\':'U':a:b:c:d:e:f:g:h:xs) = chr (fst (head (readHex [a,b,c,d,e,f,g,h]))) : go xs
    go ('\\':'\r':xs) = go (dropWhile isSpace xs)
    go ('\\':'\n':xs) = go (dropWhile isSpace xs)
    go ('\\':' ':xs)  = go (dropWhile isSpace xs)
    go ('\\':'\t':xs) = go (dropWhile isSpace xs)
    go (x:xs) = x : go xs
    go [] = error "processMlBasic: missing terminator"

processMlLiteral :: String -> String
processMlLiteral str =
  case str of
    '\'':'\'':'\'':'\r':'\n':start -> go start
    '\'':'\'':'\'':'\n':start -> go start
    '\'':'\'':'\'':start -> go start
    _ -> error "processMlLiteral: mising initializer"
  where
    go "'''" = ""
    go (x:xs) = x : go xs
    go "" = error "processMlLiteral: missing terminator"
