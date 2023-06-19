# TOML Parser

This package implements a validating parser for [TOML 1.0.0](https://toml.io/en/v1.0.0).

This package uses an [alex](https://haskell-alex.readthedocs.io/en/latest/)-generated
lexer and [happy](https://haskell-happy.readthedocs.io/en/latest/)-generated parser.

```haskell
>>> let example = "[example]\nkey = 'value'\nanother = 25\n"
>>> putStr example
[example]
key = 'value'
another = 25
>>> parse example
Right (fromList [("example",Table (fromList [("another",Integer 25),("key",String "value")]))])
```