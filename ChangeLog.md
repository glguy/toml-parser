# Revision history for toml-parser

## 1.3.0.0  --

* Make more structured error messages available in the low-level modules.
  Consumers of the `Toml` module can keep getting simple error strings
  and users interested in structured errors can run the different layers
  independently to get more detailed error reporting.
* `FromValue` and `ToValue` instances for: `Ratio`, `NonEmpty`, `Seq`

## 1.2.1.0  --  2023-07-12

* Added `Toml.Pretty.prettyTomlOrdered` to allow user-specified section ordering.
* Added `FromValue` and `ToValue` instances for `Text`
* Added `reqKeyOf` and `optKeyOf` for easier custom matching without `FromValue` instances.

## 1.2.0.0  --  2023-07-09

* Remove `FromTable` class. This class existed for things that could be
  matched specifically from tables, which is what the top-level values
  always are. However `FromValue` already handles this, and both classes
  can fail, so having the extra level of checking doesn't avoid failure.
  It does, however, create a lot of noise generating instances. Note that
  `ToTable` continues to exist because `toTable` isn't allowed to fail,
  and when serializing to TOML syntax you can only serialize top-level
  tables.
* Extracted `Toml.FromValue.Matcher` and `Toml.FromValue.ParseTable` into
  their own modules.
* Add `pickKey`, `liftMatcher`, `inKey`, `inIndex`, `parseTableFromValue` to `Toml.FromValue`
* Replace `genericFromTable` with `genericParseTable`. The intended way to
  derive a `FromValue` instance is now to write:

  ```haskell
  instance FromValue T where fromValue = parseTableFromValue genericParseTable
  ```

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
