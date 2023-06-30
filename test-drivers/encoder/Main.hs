{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Main
Description : Encoder driver for BurntSushi TOML test suite
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Encode TOML into JSON for use with <https://github.com/BurntSushi/toml-test>

-}
module Main (main) where

import Control.Applicative (empty)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (toList)
import System.Exit (exitFailure)
import Toml (prettyToml, Value(..))
import Toml.Lexer (lexValue, Token(..))

main :: IO ()
main =
 do txt <- BS.getContents
    case Aeson.decode txt of
        Nothing -> exitFailure
        Just t  -> putStr (show (prettyToml t))

instance Aeson.FromJSON Toml.Value where
    parseJSON =
        mconcat [
            Aeson.withArray "array" \xs ->
                Toml.Array <$> traverse Aeson.parseJSON (toList xs),
            Aeson.withObject "value" \o ->
                do ty <- o Aeson..: "type"
                   vl <- o Aeson..: "value"
                   decodeValue ty vl,
            fmap Toml.Table . Aeson.parseJSON
        ]

decodeValue :: String -> String -> Aeson.Parser Toml.Value
decodeValue "string"         x                                 = pure (Toml.String    x)
decodeValue "bool"           (lexValue -> TokTrue            ) = pure (Toml.Bool      True)
decodeValue "bool"           (lexValue -> TokFalse           ) = pure (Toml.Bool      False)
decodeValue "integer"        (lexValue -> TokInteger        x) = pure (Toml.Integer   x)
decodeValue "time-local"     (lexValue -> TokLocalTime      x) = pure (Toml.TimeOfDay x)
decodeValue "datetime"       (lexValue -> TokOffsetDateTime x) = pure (Toml.ZonedTime x)
decodeValue "datetime-local" (lexValue -> TokLocalDateTime  x) = pure (Toml.LocalTime x)
decodeValue "date-local"     (lexValue -> TokLocalDate      x) = pure (Toml.Day       x)
decodeValue "float"          (lexValue -> TokFloat          x) = pure (Toml.Float     x)
-- toml-tests are inconsistent about representing floating point numbers
decodeValue "float"          (lexValue -> TokInteger        x) = pure (Toml.Float (fromInteger x))
decodeValue "float"          "+Inf"                            = pure (Toml.Float (1/0))
decodeValue "float"          "-Inf"                            = pure (Toml.Float (-1/0))
decodeValue _                _                                 = empty
