{-# Language OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Main
Description : Decoder driver for BurntSushi TOML test suite
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Decode TOML into JSON for use with <https://github.com/BurntSushi/toml-test>

-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time.Format ( defaultTimeLocale, formatTime )
import System.Exit (exitFailure)
import Toml qualified
import Toml.Pretty (prettyToml)
import Control.Applicative (asum, Alternative (empty), (<|>))
import Data.Foldable (fold, Foldable (toList))
import Toml.Lexer
import Toml.Token
import Toml.Located

main :: IO ()
main =
 do txt <- BS.getContents
    case Aeson.decode txt of
        Nothing -> exitFailure
        Just t  -> putStr (prettyToml t)

instance Aeson.FromJSON Toml.Value where
    parseJSON =
        mconcat [
            Aeson.withArray "array" (\xs -> Toml.Array <$> (traverse Aeson.parseJSON (toList xs))),
            Aeson.withObject "value" (\o ->
                do ty <- o Aeson..: "type"
                   vl <- o Aeson..: "value"
                   decodeValue ty (valueToken vl) <|> decodeString ty vl  <|> decodeFlt ty vl
            ),
            \a -> Toml.Table <$> Aeson.parseJSON a
        ]

valueToken str = locThing (scanTokens ("x=" ++ str) !! 2)

decodeValue :: String -> Token -> Aeson.Parser Toml.Value
decodeValue "integer" (TokInteger i) = pure (Toml.Integer i)
decodeValue "bool"    TokTrue        = pure (Toml.Bool True)
decodeValue "bool"    TokFalse       = pure (Toml.Bool False)

decodeValue "time-local"     (TokLocalTime      x) = pure (Toml.TimeOfDay x)
decodeValue "datetime"       (TokOffsetDateTime x) = pure (Toml.ZonedTime x)
decodeValue "datetime-local" (TokLocalDateTime  x) = pure (Toml.LocalTime x)
decodeValue "date-local"     (TokLocalDate      x) = pure (Toml.Day       x)
decodeValue _ _ = empty

decodeString "string" str = pure (Toml.String str)
decodeString _ _ = empty

-- toml-tests are inconsistent about representing infinity
decodeFlt "float" "+Inf" = pure (Toml.Float (1/0))
decodeFlt "float" "-Inf" = pure (Toml.Float (-1/0))
decodeFlt "float" "+inf" = pure (Toml.Float (1/0))
decodeFlt "float" "-inf" = pure (Toml.Float (-1/0))
decodeFlt "float" "inf" = pure (Toml.Float (1/0))
decodeFlt "float" "nan" = pure (Toml.Float (0/0))
decodeFlt "float" str = pure (Toml.Float (read str))
decodeFlt _ _ = empty