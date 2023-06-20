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

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import System.Exit (exitFailure)
import Toml qualified
import Toml.Pretty (prettyValue)

main :: IO ()
main =
 do txt <- getContents
    case Toml.parse txt of
        Left{}  -> exitFailure
        Right t -> BS.putStr (Aeson.encode t)

simple :: Aeson.Key -> String -> Aeson.Value
simple ty value = Aeson.object ["type" Aeson..= ty, "value" Aeson..= value]

instance Aeson.ToJSON Toml.Value where
    toJSON v =
        case v of
            Toml.Table     t -> Aeson.toJSON t
            Toml.Array     a -> Aeson.toJSON a
            Toml.String    s -> simple "string"         s
            Toml.Integer   _ -> simple "integer"        (prettyValue v)
            Toml.Float     _ -> simple "float"          (prettyValue v)
            Toml.Bool      _ -> simple "bool"           (prettyValue v)
            Toml.TimeOfDay _ -> simple "time-local"     (prettyValue v)
            Toml.ZonedTime _ -> simple "datetime"       (prettyValue v)
            Toml.LocalTime _ -> simple "datetime-local" (prettyValue v)
            Toml.Day       _ -> simple "date-local"     (prettyValue v)
