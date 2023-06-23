{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Main
Description : Decoder driver for BurntSushi TOML test suite
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Decode TOML into JSON for use with <https://github.com/BurntSushi/toml-test>

-}
module Main (main) where

import Toml qualified
import Toml.Pretty (prettyToml, DocClass(..))
import Toml.Parser (parseRawToml)
import Toml.Lexer (scanTokens)
import System.Exit (exitFailure)
import Toml.Raw (SectionKind(..))
import Prettyprinter.Render.Terminal

main :: IO ()
main =
 do txt <- getContents
    case Toml.parse txt of
        Left{}  -> exitFailure
        Right t -> putDoc (fmap style (prettyToml t))

style TableClass = color Yellow
style NumberClass = color Cyan
style DateClass = color Green
style StringClass = color Red
style KeyClass = color Blue
style BoolClass = color Magenta