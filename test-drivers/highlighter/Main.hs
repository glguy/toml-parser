{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Decoder driver for BurntSushi TOML test suite
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Decode TOML into JSON for use with <https://github.com/BurntSushi/toml-test>

-}
module Main (main) where

import Prettyprinter.Render.Terminal
import Toml (parse, prettyToml, DocClass(..))

main :: IO ()
main =
 do txt <- getContents
    case parse txt of
        Left e  -> fail e
        Right t -> putDoc (style <$> prettyToml t)

style :: DocClass -> AnsiStyle
style TableClass  = colorDull Yellow <> bold
style NumberClass = colorDull Cyan
style DateClass   = colorDull Green
style StringClass = colorDull Red
style KeyClass    = colorDull Blue
style BoolClass   = colorDull Magenta
