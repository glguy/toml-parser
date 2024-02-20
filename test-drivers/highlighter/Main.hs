{-|
Module      : Main
Description : Decoder driver for BurntSushi TOML test suite
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Decode TOML into JSON for use with <https://github.com/BurntSushi/toml-test>

-}
module Main (main) where

import Data.Text.IO qualified as Text
import Prettyprinter.Render.Terminal
import Toml.Parser (parseRawToml)
import Toml.Pretty (prettyTomlOrdered, DocClass(..), prettyLocated, prettySemanticError)
import Toml.Semantics (semantics)
import Toml.Semantics.Ordered (extractTableOrder, projectKey)

main :: IO ()
main =
 do txt <- Text.getContents
    case parseRawToml txt of
        Left e -> fail (prettyLocated e)
        Right exprs ->
            let to = extractTableOrder exprs in
            case semantics exprs of
                Left e -> fail (prettySemanticError e)
                Right toml -> putDoc (style <$> prettyTomlOrdered (projectKey to) toml)

style :: DocClass -> AnsiStyle
style TableClass  = colorDull Yellow <> bold
style NumberClass = colorDull Cyan
style DateClass   = colorDull Green
style StringClass = colorDull Red
style KeyClass    = colorDull Blue
style BoolClass   = colorDull Magenta
