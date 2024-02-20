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
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Toml (Value(..), Value'(..), parse, Table'(..))
import Toml.Pretty (prettyValue)

main :: IO ()
main =
 do txt <- Text.getContents
    case parse txt of
        Left e  -> fail e
        Right t -> BS.putStr (Aeson.encode t)

simple :: Aeson.Key -> String -> Aeson.Value
simple ty value = Aeson.object ["type" Aeson..= ty, "value" Aeson..= value]

instance Aeson.ToJSON (Toml.Value' a) where
    toJSON v =
        case v of
            Table'     _ t -> Aeson.toJSON t
            List'      _ a -> Aeson.toJSON a
            Text'      _ s -> simple "string"         (Text.unpack s)
            Integer'   _ _ -> simple "integer"        (show (prettyValue v))
            Double'    _ _ -> simple "float"          (show (prettyValue v))
            Bool'      _ _ -> simple "bool"           (show (prettyValue v))
            TimeOfDay' _ _ -> simple "time-local"     (show (prettyValue v))
            ZonedTime' _ _ -> simple "datetime"       (show (prettyValue v))
            LocalTime' _ _ -> simple "datetime-local" (show (prettyValue v))
            Day'       _ _ -> simple "date-local"     (show (prettyValue v))

instance Aeson.ToJSON (Table' a) where
    toJSON (MkTable t) = Aeson.toJSON (fmap snd t)
