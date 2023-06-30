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
import Toml (Value(..), parse)
import Toml.Pretty (prettyValue)

main :: IO ()
main =
 do txt <- getContents
    case parse txt of
        Left e  -> fail e
        Right t -> BS.putStr (Aeson.encode t)

simple :: Aeson.Key -> String -> Aeson.Value
simple ty value = Aeson.object ["type" Aeson..= ty, "value" Aeson..= value]

instance Aeson.ToJSON Toml.Value where
    toJSON v =
        case v of
            Table     t -> Aeson.toJSON t
            Array     a -> Aeson.toJSON a
            String    s -> simple "string"         s
            Integer   _ -> simple "integer"        (show (prettyValue v))
            Float     _ -> simple "float"          (show (prettyValue v))
            Bool      _ -> simple "bool"           (show (prettyValue v))
            TimeOfDay _ -> simple "time-local"     (show (prettyValue v))
            ZonedTime _ -> simple "datetime"       (show (prettyValue v))
            LocalTime _ -> simple "datetime-local" (show (prettyValue v))
            Day       _ -> simple "date-local"     (show (prettyValue v))
