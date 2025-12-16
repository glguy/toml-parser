{-# Language OverloadedStrings, TypeOperators, TypeFamilies #-}
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
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (exitFailure)
import Toml (prettyToml, Value(..), Value'(..), Table)
import Toml.Syntax.Lexer (lexValue, Token(..))
import Toml.Schema (toValue)

main :: IO ()
main =
 do txt <- BS.getContents
    case Aeson.decode txt of
        Just (Toml.Table t) -> putStr (show (prettyToml t))
        _ -> exitFailure

instance a ~ () => Aeson.FromJSON (Toml.Value' a) where
    parseJSON =
        mconcat [
            Aeson.withArray "array" \xs ->
                Toml.List <$> traverse Aeson.parseJSON (toList xs),
            Aeson.withObject "value" \o ->
                do ty <- o Aeson..: "type"
                   vl <- o Aeson..: "value"
                   decodeValue ty vl,
            fmap (toValue :: Map.Map String Value -> Value) . Aeson.parseJSON
        ]

decodeValue :: String -> Text -> Aeson.Parser Toml.Value
decodeValue "string"         x                                         = pure (Toml.Text      x)
decodeValue "bool"           (lexValue -> Right TokTrue              ) = pure (Toml.Bool      True)
decodeValue "bool"           (lexValue -> Right TokFalse             ) = pure (Toml.Bool      False)
decodeValue "integer"        (lexValue -> Right (TokInteger        x)) = pure (Toml.Integer   x)
decodeValue "time-local"     (lexValue -> Right (TokLocalTime      x)) = pure (Toml.TimeOfDay x)
decodeValue "datetime"       (lexValue -> Right (TokOffsetDateTime x)) = pure (Toml.ZonedTime x)
decodeValue "datetime-local" (lexValue -> Right (TokLocalDateTime  x)) = pure (Toml.LocalTime x)
decodeValue "date-local"     (lexValue -> Right (TokLocalDate      x)) = pure (Toml.Day       x)
decodeValue "float"          (lexValue -> Right (TokFloat          x)) = pure (Toml.Double    x)
decodeValue "float"          (lexValue -> Right (TokInteger        x)) = pure (Toml.Double    (fromInteger x))
-- extra infinities as toml-tests are inconsistent
decodeValue "float"          "+Inf"                                    = pure (Toml.Double    (1/0))
decodeValue "float"          "-Inf"                                    = pure (Toml.Double    (-1/0))
decodeValue _                _                                         = empty
