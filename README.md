# TOML Parser

This package implements a validating parser for [TOML 1.0.0](https://toml.io/en/v1.0.0).

This package uses an [alex](https://haskell-alex.readthedocs.io/en/latest/)-generated
lexer and [happy](https://haskell-happy.readthedocs.io/en/latest/)-generated parser.

It also provides a pair of classes for serializing into and out of TOML.

## Package Structure

```mermaid
---
title: Package Structure
---
stateDiagram-v2
    classDef important font-weight:bold;

    TOML:::important --> ApplicationTypes:::important : decode
    ApplicationTypes --> TOML : encode
    TOML --> [Token]: Toml.Lexer
    [Token] --> [Expr]: Toml.Parser
    [Expr] --> Table : Toml.Semantics
    Table --> ApplicationTypes : Toml.FromTable
    ApplicationTypes --> Table : Toml.ToTable
    Table --> TOML : Toml.Pretty

```

The highest-level interface to this package is to define `FromTable` and `ToTable`
instances for your application-specific datatypes. These can be used with `encode`
and `decode` to convert to and from TOML.

For low-level access to the TOML format, the lexer, parser, and validator are available
for direct use. The diagram above shows how the different modules enable you to
advance through the increasingly high-level TOML representations.

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

Here's an example of defining datatypes and deserializers for the TOML above.

```haskell
newtype Fruits = Fruits [Fruit]
    deriving (Eq, Show)

data Fruit = Fruit String (Maybe Physical) [Variety]
    deriving (Eq, Show)

data Physical = Physical String String
    deriving (Eq, Show)

newtype Variety = Variety String
    deriving (Eq, Show)

instance FromTable Fruits where
    fromTable = runParseTable (Fruits <$> reqKey "fruits")

instance FromTable Fruit where
    fromTable = runParseTable (Fruit <$> reqKey "name" <*> optKey "physical" <*> reqKey "varieties")

instance FromTable Physical where
    fromTable = runParseTable (Physical <$> reqKey "color" <*> reqKey "shape")

instance FromTable Variety where
    fromTable = runParseTable (Variety <$> reqKey "name")

instance FromValue Fruits   where fromValue = defaultTableFromValue
instance FromValue Fruit    where fromValue = defaultTableFromValue
instance FromValue Physical where fromValue = defaultTableFromValue
instance FromValue Variety  where fromValue = defaultTableFromValue
```

We can run this example on the original value to deserialize it into domain-specific datatypes.

```haskell
>>> decode fruitToml :: Either String Fruits
Right (Fruits [
    Fruit "apple" (Just (Physical "red" "round")) [Variety "red delicious", Variety "granny smith"],
    Fruit "banana" Nothing [Variety "plantain"]])
```
