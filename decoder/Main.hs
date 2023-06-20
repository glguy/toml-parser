{-# Language OverloadedStrings #-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson

import Toml qualified
import System.Exit (exitFailure)
import Data.ByteString.Lazy qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String
import Toml.Pretty
import Data.Text (Text, pack)
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Vector qualified as Vector

main :: IO ()
main =
 do txt <- getContents
    case Toml.parse txt of
        Left{} -> exitFailure
        Right t ->
            BS.putStr (Aeson.encode (encodeTable t))

encodeTable :: Map String Toml.Value -> Aeson.Value
encodeTable m =
    Aeson.Object (Aeson.fromList [
        (fromString k, encodeValue v)
        | (k,v) <- Map.assocs m
    ])

encodeValue :: Toml.Value -> Aeson.Value
encodeValue = \case
    Toml.Integer i -> simple "integer" (show i)
    Toml.Float x
      | isNaN x -> simple "float" "nan"
      | isInfinite x -> simple "float" (if x > 0 then "inf" else "-inf")
      | otherwise -> simple "float" (show x)
    Toml.String x -> simple "string" x
    Toml.Bool True -> simple "bool" "true"
    Toml.Bool False -> simple "bool" "false"
    Toml.TimeOfDay x -> simple "time-local" (formatTime defaultTimeLocale "%H:%M:%S%Q" x)
    Toml.ZonedTime x -> simple "datetime" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" x)
    Toml.LocalTime x -> simple "datetime-local" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%" x)
    Toml.Day x       -> simple "date-local" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" x)
    Toml.Table t -> encodeTable t
    Toml.Array a -> Aeson.Array (Vector.fromList (map encodeValue a))

simple :: Text -> String -> Aeson.Value
simple ty val = Aeson.Object (Aeson.fromList [("type", Aeson.String ty), ("value", Aeson.String (pack val))])