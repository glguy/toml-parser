# Revision history for toml-parser

## 1.1.2.0  --

* Add `pickKey`, `liftMatcher`, `inKey`, `inIndex` to `Toml.FromValue`

## 1.1.1.0  --  2023-07-03

* Add support for GHC 8.10.7 and 9.0.2

## 1.1.0.0  --  2023-07-03

* Add Toml.FromValue.Generic and Toml.ToValue.Generic
* Add Alternative instance to Matcher and support multiple error messages in Result
* Add Data and Generic instances for Value

## 1.0.1.0  -- 2023-07-01

* Add ToTable and ToValue instances for Map
* Refine error messages
* More test coverage

## 1.0.0.0  -- 2023-06-29

* Complete rewrite including 1.0.0 compliance and pretty-printing.

## 0.1.0.0  -- 2017-05-04

* First version.
