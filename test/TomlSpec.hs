{-# Language QuasiQuotes #-}
{-|
Module      : TomlSpec
Description : Unit tests
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

TOML parser and validator unit tests (primarily drawn from the
specification document).

-}
module TomlSpec (spec) where

import Data.Map qualified as Map
import Data.Time (Day)
import QuoteStr (quoteStr)
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Toml (Value(..), parse, decode, Result(Success))
import Toml.ToValue (table, (.=))

spec :: Spec
spec =
 do describe "comment"
     do it "ignores comments" $
          parse [quoteStr|
            # This is a full-line comment
            key = "value"  # This is a comment at the end of a line
            another = "# This is not a comment"|]
          `shouldBe`
          Right (table [("another",String "# This is not a comment"),("key",String "value")])

    describe "key/value pair"
     do it "supports the most basic assignments" $
          parse "key = \"value\"" `shouldBe` Right (Map.singleton "key" (String "value"))

        it "requires a value after equals" $
          parse "key = # INVALID"
          `shouldBe`
          Left "1:16: parse error: unexpected end-of-input"

        it "requires newlines between assignments" $
          parse "first = \"Tom\" last = \"Preston-Werner\" # INVALID"
          `shouldBe`
          Left "1:15: parse error: unexpected bare key"

    describe "keys"
     do it "allows bare keys" $
          parse [quoteStr|
            key = "value"
            bare_key = "value"
            bare-key = "value"
            1234 = "value"|]
          `shouldBe`
          Right (table [
            "1234"     .= "value",
            "bare-key" .= "value",
            "bare_key" .= "value",
            "key"      .= "value"])

        it "allows quoted keys" $
          parse [quoteStr|
            "127.0.0.1" = "value"
            "character encoding" = "value"
            "ʎǝʞ" = "value"
            'key2' = "value"
            'quoted "value"' = "value"|]
          `shouldBe`
          Right (table [
            "127.0.0.1"          .= "value",
            "character encoding" .= "value",
            "key2"               .= "value",
            "quoted \"value\""   .= "value",
            "ʎǝʞ"                .= "value"])

        it "allows dotted keys" $
          parse [quoteStr|
            name = "Orange"
            physical.color = "orange"
            physical.shape = "round"
            site."google.com" = true|]
          `shouldBe`
          Right (table [
            "name"     .= "Orange",
            "physical" .= table ["color" .= "orange", "shape" .= "round"],
            "site"     .= table ["google.com" .= True]])

        it "prevents duplicate keys" $
          parse [quoteStr|
            name = "Tom"
            name = "Pradyun"|]
          `shouldBe` Left "2:1: key error: name is already assigned"

        it "prevents duplicate keys even between bare and quoted" $
          parse [quoteStr|
            spelling = "favorite"
            "spelling" = "favourite"|]
          `shouldBe` Left "2:1: key error: spelling is already assigned"

        it "allows out of order definitions" $
          parse [quoteStr|
            apple.type = "fruit"
            orange.type = "fruit"

            apple.skin = "thin"
            orange.skin = "thick"

            apple.color = "red"
            orange.color = "orange"|]
          `shouldBe`
          Right (table [
            "apple" .= table [
                "color" .= "red",
                "skin"  .= "thin",
                "type"  .= "fruit"],
            "orange" .= table [
                "color" .= "orange",
                "skin"  .= "thick",
                "type"  .= "fruit"]])

        it "allows numeric bare keys" $
          parse "3.14159 = 'pi'" `shouldBe` Right (table [
            "3" .= table [("14159", String "pi")]])

        it "allows keys that look like other values" $
          parse [quoteStr|
            true = true
            false = false
            1900-01-01 = 1900-01-01
            1_2 = 2_3|]
          `shouldBe`
          Right (table [
            "1900-01-01" .= (read "1900-01-01" :: Day),
            "1_2"        .= (23::Int),
            "false"      .= False,
            "true"       .= True])

    describe "string"
     do it "parses escapes" $
          parse [quoteStr|
            str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."|]
          `shouldBe`
          Right (Map.singleton "str" (String "I'm a string. \"You can quote me\". Name\tJos\xe9\nLocation\tSF."))

        it "strips the initial newline from multiline strings" $
          parse [quoteStr|
            str1 = """
            Roses are red
            Violets are blue"""|]
          `shouldBe` Right (Map.singleton "str1" (String "Roses are red\nViolets are blue"))

        it "strips whitespace with a trailing escape" $
          parse [quoteStr|
            # The following strings are byte-for-byte equivalent:
            str1 = "The quick brown fox jumps over the lazy dog."

            str2 = """
            The quick brown \


            fox jumps over \
                the lazy dog."""

            str3 = """\
                The quick brown \
                fox jumps over \
                the lazy dog.\
                """|]
          `shouldBe`
          Right (table [
            "str1" .= "The quick brown fox jumps over the lazy dog.",
            "str2" .= "The quick brown fox jumps over the lazy dog.",
            "str3" .= "The quick brown fox jumps over the lazy dog."])

        it "allows quotes inside multiline quoted strings" $
          parse [quoteStr|
            str4 = """Here are two quotation marks: "". Simple enough."""
            str5 = """Here are three quotation marks: ""\"."""
            str6 = """Here are fifteen quotation marks: ""\"""\"""\"""\"""\"."""

            # "This," she said, "is just a pointless statement."
            str7 = """"This," she said, "is just a pointless statement.""""|]
          `shouldBe`
          Right (table [
            "str4" .= "Here are two quotation marks: \"\". Simple enough.",
            "str5" .= "Here are three quotation marks: \"\"\".",
            "str6" .= "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\".",
            "str7" .= "\"This,\" she said, \"is just a pointless statement.\""])

        it "disallows triple quotes inside a multiline string" $
          parse [quoteStr|
            str5 = """Here are three quotation marks: """."""  # INVALID|]
          `shouldBe` Left "1:46: parse error: unexpected '.'"

        it "ignores escapes in literal strings" $
          parse [quoteStr|
            # What you see is what you get.
            winpath  = 'C:\Users\nodejs\templates'
            winpath2 = '\\ServerX\admin$\system32\'
            quoted   = 'Tom "Dubs" Preston-Werner'
            regex    = '<\i\c*\s*>'|]
          `shouldBe`
          Right (table [
            "quoted"   .= "Tom \"Dubs\" Preston-Werner",
            "regex"    .= "<\\i\\c*\\s*>",
            "winpath"  .= "C:\\Users\\nodejs\\templates",
            "winpath2" .= "\\\\ServerX\\admin$\\system32\\"])

        it "handles multiline literal strings" $
          parse [quoteStr|
            regex2 = '''I [dw]on't need \d{2} apples'''
            lines  = '''
            The first newline is
            trimmed in raw strings.
            All other whitespace
            is preserved.
            '''|]
          `shouldBe`
          Right (table [
            "lines"  .= "The first newline is\ntrimmed in raw strings.\nAll other whitespace\nis preserved.\n",
            "regex2" .= "I [dw]on't need \\d{2} apples"])

        it "parses all the other escapes" $
          parse [quoteStr|
            x = "\\\b\f\r\U0010abcd"
            y = """\\\b\f\r\u7bca\U0010abcd\n\r\t"""|]
          `shouldBe`
          Right (table [
            "x" .= "\\\b\f\r\x0010abcd",
            "y" .= "\\\b\f\r\x7bca\x0010abcd\n\r\t"])

        it "rejects out of range unicode escapes" $
          parse [quoteStr|
            x = "\U11111111"|]
          `shouldBe` Left "1:6: lexical error: unicode escape too large"

        it "handles unexpected end of line" $
          parse [quoteStr|
            x = "example
            y = 42|]
          `shouldBe` Left "1:13: lexical error: unexpected end-of-line"

    describe "integer"
     do it "parses literals correctly" $
          parse [quoteStr|
            int1 = +99
            int2 = 42
            int3 = 0
            int4 = -17
            int5 = 1_000
            int6 = 5_349_221
            int7 = 53_49_221  # Indian number system grouping
            int8 = 1_2_3_4_5  # VALID but discouraged
            # hexadecimal with prefix `0x`
            hex1 = 0xDEADBEEF
            hex2 = 0xdeadbeef
            hex3 = 0xdead_beef

            # octal with prefix `0o`
            oct1 = 0o01234567
            oct2 = 0o755 # useful for Unix file permissions

            # binary with prefix `0b`
            bin1 = 0b11010110|]
          `shouldBe` Right
          (table [
              "bin1" .= Integer 214,
              "hex1" .= Integer 0xDEADBEEF,
              "hex2" .= Integer 0xDEADBEEF,
              "hex3" .= Integer 0xDEADBEEF,
              "int1" .= Integer 99,
              "int2" .= Integer 42,
              "int3" .= Integer 0,
              "int4" .= Integer (-17),
              "int5" .= Integer 1000,
              "int6" .= Integer 5349221,
              "int7" .= Integer 5349221,
              "int8" .= Integer 12345,
              "oct1" .= Integer 0o01234567,
              "oct2" .= Integer 0o755])

    it "handles leading zeros gracefully" $
      parse "x = 01"
      `shouldBe`
      Left "1:5: lexical error: leading zero prohibited"


    describe "float"
     do it "parses floats" $
          parse [quoteStr|
            # fractional
            flt1 = +1.0
            flt2 = 3.1415
            flt3 = -0.01

            # exponent
            flt4 = 5e+22
            flt5 = 1e06
            flt6 = -2E-2

            # both
            flt7 = 6.626e-34
            flt8 = 224_617.445_991_228
            # infinity
            sf1 = inf  # positive infinity
            sf2 = +inf # positive infinity
            sf3 = -inf # negative infinity|]
          `shouldBe`
          Right (table [
            "flt1" .= Float 1.0,
            "flt2" .= Float 3.1415,
            "flt3" .= Float (-1.0e-2),
            "flt4" .= Float 4.9999999999999996e22,
            "flt5" .= Float 1000000.0,
            "flt6" .= Float (-2.0e-2),
            "flt7" .= Float 6.626e-34,
            "flt8" .= Float 224617.445991228,
            "sf1"  .= Float (1/0),
            "sf2"  .= Float (1/0),
            "sf3"  .= Float (-1/0)])

        it "parses nan correctly" $
          let checkNaN (Float x) = isNaN x
              checkNaN _         = False
          in
          parse [quoteStr|
            # not a number
            sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific
            sf5 = +nan # same as `nan`
            sf6 = -nan # valid, actual encoding is implementation-specific|]
          `shouldSatisfy` \case
            Left{} -> False
            Right x -> all checkNaN x

        -- code using Numeric.readFloat can use significant
        -- resources. this makes sure this doesn't start happening
        -- in the future
        it "parses huge floats without great delays" $
          parse "x = 1e1000000000000"
          `shouldBe`
          Right (Map.singleton "x" (Float (1/0)))

    describe "boolean"
     do it "parses boolean literals" $
          parse [quoteStr|
            bool1 = true
            bool2 = false|]
          `shouldBe`
          Right (table [
            "bool1" .= True,
            "bool2" .= False])

    describe "offset date-time"
     do it "parses offset date times" $
          parse [quoteStr|
            odt1 = 1979-05-27T07:32:00Z
            odt2 = 1979-05-27T00:32:00-07:00
            odt3 = 1979-05-27T00:32:00.999999-07:00
            odt4 = 1979-05-27 07:32:00Z|]
          `shouldBe`
          Right (table [
            "odt1" .= ZonedTime (read "1979-05-27 07:32:00 +0000"),
            "odt2" .= ZonedTime (read "1979-05-27 00:32:00 -0700"),
            "odt3" .= ZonedTime (read "1979-05-27 00:32:00.999999 -0700"),
            "odt4" .= ZonedTime (read "1979-05-27 07:32:00 +0000")])

    describe "local date-time"
     do it "parses local date-times" $
          parse [quoteStr|
            ldt1 = 1979-05-27T07:32:00
            ldt2 = 1979-05-27T00:32:00.999999
            ldt3 = 1979-05-28 00:32:00.999999|]
          `shouldBe`
          Right (table [
            "ldt1" .= LocalTime (read "1979-05-27 07:32:00"),
            "ldt2" .= LocalTime (read "1979-05-27 00:32:00.999999"),
            "ldt3" .= LocalTime (read "1979-05-28 00:32:00.999999")])

        it "catches invalid date-times" $
          parse [quoteStr|
            ldt = 9999-99-99T99:99:99|]
          `shouldBe`
          Left "1:7: lexical error: malformed local date-time"

    describe "local date"
     do it "parses dates" $
          parse [quoteStr|
            ld1 = 1979-05-27|]
          `shouldBe`
          Right (Map.singleton "ld1" (Day (read "1979-05-27")))

    describe "local time"
     do it "parses times" $
          parse [quoteStr|
            lt1 = 07:32:00
            lt2 = 00:32:00.999999|]
          `shouldBe`
          Right (table [
            "lt1" .= TimeOfDay (read "07:32:00"),
            "lt2" .= TimeOfDay (read "00:32:00.999999")])

    describe "array"
     do it "parses array examples" $
          parse [quoteStr|
            integers = [ 1, 2, 3 ]
            colors = [ "red", "yellow", "green" ]
            nested_arrays_of_ints = [ [ 1, 2 ], [3, 4, 5] ]
            nested_mixed_array = [ [ 1, 2 ], ["a", "b", "c"] ]
            string_array = [ "all", 'strings', """are the same""", '''type''' ]

            # Mixed-type arrays are allowed
            numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
            contributors = [
            "Foo Bar <foo@example.com>",
            { name = "Baz Qux", email = "bazqux@example.com", url = "https://example.com/bazqux" }
            ]|]
            `shouldBe`
            Right (table [
                "colors" .= ["red", "yellow", "green"],
                "contributors" .= [
                    String "Foo Bar <foo@example.com>",
                    Table (table [
                        "email" .= "bazqux@example.com",
                        "name" .= "Baz Qux",
                        "url" .= "https://example.com/bazqux"])],
                "integers" .= [1, 2, 3 :: Integer],
                "nested_arrays_of_ints" .= [[1, 2], [3, 4, 5 :: Integer]],
                "nested_mixed_array" .= [[Integer 1, Integer 2], [String "a", String "b", String "c"]],
                "numbers" .= [Float 0.1, Float 0.2, Float 0.5, Integer 1, Integer 2, Integer 5],
                "string_array" .= ["all", "strings", "are the same", "type"]])

        it "handles newlines and comments" $
          parse [quoteStr|
            integers2 = [
            1, 2, 3
            ]

            integers3 = [
            1,
            2, # this is ok
            ]|]
            `shouldBe`
            Right (table [
                "integers2" .= [1, 2, 3 :: Int],
                "integers3" .= [1, 2 :: Int]])

        it "disambiguates double brackets from array tables" $
          parse "x = [[1]]" `shouldBe` Right (Map.singleton "x" (Array [Array [Integer 1]]))

    describe "table"
     do it "allows empty tables" $
          parse "[table]" `shouldBe` Right (table ["table" .= table []])

        it "parses simple tables" $
          parse [quoteStr|
            [table-1]
            key1 = "some string"
            key2 = 123

            [table-2]
            key1 = "another string"
            key2 = 456|]
          `shouldBe`
          Right (table [
            "table-1" .= table [
                "key1" .= "some string",
                "key2" .= Integer 123],
            "table-2" .= table [
                "key1" .= "another string",
                "key2" .= Integer 456]])

        it "allows quoted keys" $
          parse [quoteStr|
            [dog."tater.man"]
            type.name = "pug"|]
          `shouldBe`
          Right (table ["dog" .= table ["tater.man" .= table ["type" .= table ["name" .= "pug"]]]])

        it "allows whitespace around keys" $
          parse [quoteStr|
            [a.b.c]            # this is best practice
            [ d.e.f ]          # same as [d.e.f]
            [ g .  h  . i ]    # same as [g.h.i]
            [ j . "ʞ" . 'l' ]  # same as [j."ʞ".'l']|]
          `shouldBe`
          Right (table [
            "a" .= table ["b" .= table ["c" .= table []]],
            "d" .= table ["e" .= table ["f" .= table []]],
            "g" .= table ["h" .= table ["i" .= table []]],
            "j" .= table ["ʞ" .= table ["l" .= table []]]])

        it "allows supertables to be defined after subtables" $
          parse [quoteStr|
            # [x] you
            # [x.y] don't
            # [x.y.z] need these
            [x.y.z.w] # for this to work

            [x] # defining a super-table afterward is ok
            q=1|]
          `shouldBe`
          Right (table [
            "x" .= table [
                "q" .= Integer 1,
                "y" .= table [
                    "z" .= table [
                        "w" .= table []]]]])

        it "prevents using a [table] to open a table defined with dotted keys" $
          parse [quoteStr|
            [fruit]
            apple.color = 'red'
            apple.taste.sweet = true
            [fruit.apple]|]
          `shouldBe` Left "4:8: key error: apple is a closed table"

        it "can add subtables" $
          parse [quoteStr|
            [fruit]
            apple.color = "red"
            apple.taste.sweet = true
            [fruit.apple.texture]  # you can add sub-tables
            smooth = true|]
          `shouldBe`
          Right (table [
            "fruit" .= table [
                "apple" .= table [
                    "color" .= "red",
                    "taste" .= table [
                        "sweet" .= True],
                        "texture" .= table [
                            "smooth" .= True]]]])

    describe "inline table"
     do it "parses inline tables" $
          parse [quoteStr|
            name = { first = "Tom", last = "Preston-Werner" }
            point = { x = 1, y = 2 }
            animal = { type.name = "pug" }|]
          `shouldBe`
          Right (table [
            "animal" .= table ["type" .= table ["name" .= "pug"]],
            "name"   .= table ["first" .= "Tom", "last" .= "Preston-Werner"],
            "point"  .= table ["x" .= Integer 1, "y" .= Integer 2]])

        it "prevents altering inline tables with dotted keys" $
          parse [quoteStr|
            [product]
            type = { name = "Nail" }
            type.edible = false  # INVALID|]
          `shouldBe` Left "3:1: key error: type is already assigned"

        it "prevents using inline tables to add keys to existing tables" $
          parse [quoteStr|
            [product]
            type.name = "Nail"
            type = { edible = false }  # INVALID|]
          `shouldBe` Left "3:1: key error: type is already assigned"

        it "checks that inline keys aren't reassigned" $
          parse [quoteStr|
            x = {a = 1, a = 2}|]
          `shouldBe` Left "1:13: key error: a is already assigned"

        it "checks that inline keys don't overlap with implicit inline tables" $
          parse [quoteStr|
            x = {a.b = 1, a = 2}|]
          `shouldBe` Left "1:15: key error: a is already assigned"
        
        it "checks for overwrites from other inline tables" $
          parse [quoteStr|
            tab = { inner = { dog = "best" }, inner.cat = "worst" }|]
          `shouldBe` Left "1:35: key error: inner is already assigned"
        
        it "checks for overlaps of other inline tables" $
          parse [quoteStr|
            tbl = { fruit = { apple.color = "red" }, fruit.apple.texture = { smooth = true } }|]
          `shouldBe` Left "1:42: key error: fruit is already assigned"

    describe "array of tables"
     do it "supports array of tables syntax" $
          decode [quoteStr|
            [[products]]
            name = "Hammer"
            sku = 738594937

            [[products]]  # empty table within the array

            [[products]]
            name = "Nail"
            sku = 284758393

            color = "gray"|]
          `shouldBe`
          Success mempty (Map.singleton "products" [
            table [
              "name" .= "Hammer",
              "sku"  .= Integer 738594937],
            Map.empty,
            table [
                "color" .= "gray",
                "name"  .= "Nail",
                "sku"   .= Integer 284758393]])

        it "handles subtables under array of tables" $
          parse [quoteStr|
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
            name = "plantain"|]
          `shouldBe`
          Right (table [
            "fruits" .= [
                table [
                    "name" .= "apple",
                    "physical" .= table [
                        "color" .= "red",
                        "shape" .= "round"],
                    "varieties" .= [
                        table ["name" .= "red delicious"],
                        table ["name" .= "granny smith"]]],
                table [
                    "name" .= "banana",
                    "varieties" .= [
                        table ["name" .= "plantain"]]]]])

        it "prevents redefining a supertable with an array of tables" $
          parse [quoteStr|
            # INVALID TOML DOC
            [fruit.physical]  # subtable, but to which parent element should it belong?
            color = "red"
            shape = "round"

            [[fruit]]  # parser must throw an error upon discovering that "fruit" is
                    # an array rather than a table
            name = "apple"|]
            `shouldBe` Left "6:3: key error: fruit is already implicitly defined to be a table"

        it "prevents redefining an inline array" $
          parse [quoteStr|
            # INVALID TOML DOC
            fruits = []

            [[fruits]] # Not allowed|]
          `shouldBe` Left "4:3: key error: fruits is already assigned"

    -- these cases are needed to complete coverage checking on Semantics module
    describe "corner cases"
     do it "stays open" $
          parse [quoteStr|
            [x.y.z]
            [x]
            [x.y]|]
          `shouldBe`
          parse "x.y.z={}"

        it "stays closed" $
          parse [quoteStr|
            [x.y]
            [x]
            [x.y]|] `shouldBe` Left "3:4: key error: y is a closed table"

        it "super tables of array tables preserve array tables" $
          parse [quoteStr|
            [[x.y]]
            [x]
            [[x.y]]|]
          `shouldBe`
          parse "x.y=[{},{}]"

        it "super tables of array tables preserve array tables" $
          parse [quoteStr|
            [[x.y]]
            [x]
            [x.y.z]|]
          `shouldBe`
          parse "x.y=[{z={}}]"

        it "detects conflicting inline keys" $
          parse [quoteStr|
            x = { y = 1, y.z = 2}|]
          `shouldBe` Left "1:14: key error: y is already assigned"

        it "handles merging dotted inline table keys" $
          parse [quoteStr|
            t = { a.x.y = 1, a.x.z = 2, a.q = 3}|]
          `shouldBe`
          Right (table [
            "t" .= table [
                "a" .= table [
                    "q" .= Integer 3,
                    "x" .= table [
                        ("y",Integer 1),
                        ("z",Integer 2)]]]])

        it "disallows overwriting assignments with tables" $
          parse [quoteStr|
            x = 1
            [x.y]|]
          `shouldBe` Left "2:2: key error: x is already assigned"

        it "handles super super tables" $
          parse [quoteStr|
            [x.y.z]
            [x.y]
            [x]|]
          `shouldBe`
          parse "x.y.z={}"

        it "You can dot into open supertables" $
          parse [quoteStr|
            [x.y.z]
            [x]
            y.q = 1|]
          `shouldBe`
          parse "x.y={z={},q=1}"

        it "dotted tables close previously open tables" $
          parse [quoteStr|
            [x.y.z]
            [x]
            y.q = 1
            [x.y]|]
          `shouldBe` Left "4:4: key error: y is a closed table"

        it "dotted tables can't assign through closed tables!" $
          parse [quoteStr|
            [x.y]
            [x]
            y.z.w = 1|]
          `shouldBe` Left "3:1: key error: y is a closed table"

        it "super tables can't add new subtables to array tables via dotted keys" $
          parse [quoteStr|
            [[x.y]]
            [x]
            y.z.a = 1
            y.z.b = 2|]
          `shouldBe` Left "3:1: key error: y is a closed table"

        it "the previous example preserves closeness" $
          parse [quoteStr|
            [[x.y]]
            [x]
            y.z.a = 1
            y.w = 2|]
          `shouldBe` Left "3:1: key error: y is a closed table"

        it "defining a supertable closes the supertable" $
          parse [quoteStr|
            [x.y]
            [x]
            [x]|]
          `shouldBe` Left "3:2: key error: x is a closed table"

        it "prevents redefining an array of tables" $
          parse [quoteStr|
            [[x.y]]
            [x.y]|]
          `shouldBe` Left "2:4: key error: y is a closed table"

        it "quotes table names in semantic errors" $
          parse [quoteStr|
            [[x.""]]
            [x.""]|]
          `shouldBe` Left "2:4: key error: \"\" is a closed table"
