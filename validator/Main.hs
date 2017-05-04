{-|
Module      : Main
Description : toml-test validator harness
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main (main) where

import           Control.Exception
import           Data.Text (Text)
import           Data.Time
import           TOML
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

main =
  do txt  <- Text.getContents
     toml <- either throwIO return (parseTOML txt)
     B.putStr (A.encode (convertTable toml))

convertTable :: [(Text,Value)] -> A.Value
convertTable t = A.toJSON (Map.fromList [ (k, convertValue v) | (k,v) <- t])

convertValue :: Value -> A.Value
convertValue v =
  case v of
    ListV xs     -> A.toJSON (map convertValue xs)
    TableV t     -> convertTable t
    DoubleV d    -> convertLeaf "float" (show d)
    IntegerV x   -> convertLeaf "integer" (show x)
    StringV s    -> convertLeaf "string" s
    BoolV x      -> convertLeaf "bool" (if x then "true" else "false")
    ZonedTimeV x -> convertDatetime x (iso8601DateFormat (Just "%T%Q%Z"))
    LocalTimeV x -> convertDatetime x (iso8601DateFormat (Just "%T%Q"))
    DayV       x -> convertDatetime x (iso8601DateFormat Nothing)
    TimeOfDayV x -> convertDatetime x "%T%Q"

convertDatetime :: FormatTime t => t -> String -> A.Value
convertDatetime x fmt =
  convertLeaf "datetime" (formatTime defaultTimeLocale fmt x)

convertLeaf :: A.ToJSON a => String -> a -> A.Value
convertLeaf tyname val =
  A.toJSON (Map.fromList [("type", A.toJSON tyname), ("value", A.toJSON val)])
