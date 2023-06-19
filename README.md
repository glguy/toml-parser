# TOML Parser

This package implements a validating parser for [TOML 1.0.0](https://toml.io/en/v1.0.0).

This package uses an [alex](https://haskell-alex.readthedocs.io/en/latest/)-generated
lexer and [happy](https://haskell-happy.readthedocs.io/en/latest/)-generated parser.

## Example

Consider this sample TOML text from the specification.

```toml
[[fruits]]
name = "apple"

[fruits.physical]  # subtable
color = "red"
shape = "round"

[[fruits.varieties]]  # nested array of tables
name = "red delicious"

[[fruits.varieties]]
name = "granny smith"


[[fruits]]
name = "banana"

[[fruits.varieties]]
name = "plantain"
```

Parsing using this package generates the following value

```haskell
>>> Right fruitToml = parse fruitStr
>>> fruitToml
Right (fromList [
    ("fruits",Array [
        Table (fromList [
            ("name",String "apple"),
            ("physical",Table (fromList [
                ("color",String "red"),
                ("shape",String "round")])),
            ("varieties",Array [
                Table (fromList [("name",String "red delicious")]),
                Table (fromList [("name",String "granny smith")])])]),
        Table (fromList [
            ("name",String "banana"),
            ("varieties",Array [
                Table (fromList [("name",String "plantain")])])])])])
```

We can render this parsed value back to TOML text using `prettyToml fruitToml`.
In this case the input was already sorted, so the generated text will happen
to match almost exactly.
