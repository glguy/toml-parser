{-|
Module      : Main
Description : toml-test validator harness
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main (main) where

import           Control.Exception
import qualified Data.Aeson as A
import           Data.Text (Text)
import           Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Text.IO as Text

import           TOML

main :: IO ()
main =
  do txt  <- Text.getContents
     toml <- either throwIO return (parseTOML txt)
     B.putStr (A.encode (convertTable toml))

convertTable :: [(Text,Value)] -> A.Value
convertTable t = A.toJSON (Map.fromList [ (k, convertValue v) | (k,v) <- t])

convertValue :: Value -> A.Value
convertValue v =
  case v of
    List       x -> A.toJSON (map convertValue x)
    Table      x -> convertTable x
    Double     x -> convertLeaf "float" (show x)
    Integer    x -> convertLeaf "integer" (show x)
    String     x -> convertLeaf "string" x
    Bool       x -> convertLeaf "bool" (if x then "true" else "false")
    ZonedTimeV x -> convertLeaf "datetime" (iso8601Show x)
    LocalTimeV x -> convertLeaf "datetime-local" (iso8601Show x)
    DayV       x -> convertLeaf "date-local" (iso8601Show x)
    TimeOfDayV x -> convertLeaf "time-local" (iso8601Show x)

convertLeaf :: A.ToJSON a => String -> a -> A.Value
convertLeaf tyname val =
  A.toJSON (Map.fromList [("type", A.toJSON tyname), ("value", A.toJSON val)])
