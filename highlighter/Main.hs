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

style :: DocClass -> AnsiStyle
style TableClass  = colorDull Yellow <> bold
style NumberClass = colorDull Cyan
style DateClass   = colorDull Green
style StringClass = colorDull Red
style KeyClass    = colorDull Blue
style BoolClass   = colorDull Magenta
