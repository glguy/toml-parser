{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Unit tests
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

TOML parser and validator unit tests (primarily drawn from the
specification document).

-}
module Main (main) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time (Day, TimeOfDay, LocalTime, ZonedTime)
import QuoteStr (quoteStr)
import Test.Hspec (hspec, describe, it, shouldBe, shouldSatisfy, Spec)
import Toml (Value(..), parse, decode, encode, Result(Success), prettyToml, Table)
import Toml.FromValue (FromValue(..), defaultTableFromValue, reqKey, optKey, runParseTable, ParseTable, FromTable (fromTable))
import Toml.ToValue (table, (.=), toValue)

main :: IO ()
main = hspec do

  describe "lexer"
   do it "handles special cased control character" $
        parse "x = '\SOH'"
        `shouldBe`
        Left "1:6: lexical error: unexpected '\\SOH'"

      -- These seem boring, but they provide test coverage of an error case in the state machine
      it "handles unexpected '}'" $
        parse "}"
        `shouldBe`
        Left "1:1: parse error: unexpected '}'"

      it "handles unexpected '{'" $
        parse "{"
        `shouldBe`
        Left "1:1: parse error: unexpected '{'"

      it "accepts tabs" $
        parse "x\t=\t1"
        `shouldBe`
        Right (Map.singleton "x" (Integer 1))

      it "computes columns correctly with tabs" $
        parse "x\t=\t="
        `shouldBe`
        Left "1:17: parse error: unexpected '='"

      it "detects non-scalars in strings" $
        parse "x = \"\\udfff\""
        `shouldBe`
        Left "1:6: lexical error: non-scalar unicode escape"

      it "catches unclosed [" $
        parse "x = [1,2,3"
        `shouldBe`
        Left "1:5: lexical error: unterminated '['"

      it "catches unclosed {" $
        parse "x = { y"
        `shouldBe`
        Left "1:5: lexical error: unterminated '{'"

      it "catches unclosed \"" $
        parse "x = \"abc"
        `shouldBe`
        Left "1:5: lexical error: unterminated string literal"

      it "catches unclosed \"\"\"" $
        parse "x = \"\"\"test"
        `shouldBe`
        Left "1:5: lexical error: unterminated multi-line string literal"
      
      it "handles escapes at the end of input" $
        parse "x = \"\\"
        `shouldBe`
        Left "1:7: lexical error: unexpected end-of-input"

      it "handles invalid escapes" $
        parse "x = \"\\p\""
        `shouldBe`
        Left "1:7: lexical error: unexpected 'p'"

  describe "ToValue"
   do
    it "converts characters as singleton strings" $
      toValue '!' `shouldBe` String "!"

  describe "parse" do
    describe "comment"
     do it "ignores comments" $
          parse [quoteStr|
            # This is a full-line comment
            key = "value"  # This is a comment at the end of a line
            another = "# This is not a comment"|]
          `shouldBe`
          Right (Map.fromList [("another",String "# This is not a comment"),("key",String "value")])

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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
            "apple" .= table [
                "color" .= "red",
                "skin"  .= "thin",
                "type"  .= "fruit"],
            "orange" .= table [
                "color" .= "orange",
                "skin"  .= "thick",
                "type"  .= "fruit"]])

        it "allows numeric bare keys" $
          parse "3.14159 = 'pi'" `shouldBe` Right (Map.singleton "3" (table [("14159", String "pi")]))

        it "allows keys that look like other values" $
          parse [quoteStr|
            true = true
            false = false
            1900-01-01 = 1900-01-01
            1_2 = 2_3|]
          `shouldBe`
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
            "lines"  .= "The first newline is\ntrimmed in raw strings.\nAll other whitespace\nis preserved.\n",
            "regex2" .= "I [dw]on't need \\d{2} apples"])

        it "parses all the other escapes" $
          parse [quoteStr|
            x = "\\\b\f\r\U0010abcd"
            y = """\\\b\f\r\u7bca\U0010abcd\n\r\t"""|]
          `shouldBe`
          Right (Map.fromList [
            "x" .= "\\\b\f\r\x0010abcd",
            "y" .= "\\\b\f\r\x7bca\x0010abcd\n\r\t"])

        it "rejects out of range unicode escapes" $
          parse [quoteStr|
            x = "\U11111111"|]
          `shouldBe` Left "1:6: lexical error: unicode escape too large"

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
          (Map.fromList [
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
          Right (Map.fromList [
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

    describe "boolean"
     do it "parses boolean literals" $
          parse [quoteStr|
            bool1 = true
            bool2 = false|]
          `shouldBe`
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
            Right (Map.fromList [
                "colors" .= ["red", "yellow", "green"],
                "contributors" .= [
                    String "Foo Bar <foo@example.com>",
                    table [
                        "email" .= "bazqux@example.com",
                        "name" .= "Baz Qux",
                        "url" .= "https://example.com/bazqux"]],
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
            Right (Map.fromList [
                "integers2" .= [1, 2, 3 :: Int],
                "integers3" .= [1, 2 :: Int]])

        it "disambiguates double brackets from array tables" $
          parse "x = [[1]]" `shouldBe` Right (Map.singleton "x" (Array [Array [Integer 1]]))

    describe "table"
     do it "allows empty tables" $
          parse "[table]" `shouldBe` Right (Map.singleton "table" (table []))

        it "parses simple tables" $
          parse [quoteStr|
            [table-1]
            key1 = "some string"
            key2 = 123

            [table-2]
            key1 = "another string"
            key2 = 456|]
          `shouldBe`
          Right (Map.fromList [
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
          Right (Map.fromList [("dog", table [("tater.man", table [("type", table [("name",String "pug")])])])])

        it "allows whitespace around keys" $
          parse [quoteStr|
            [a.b.c]            # this is best practice
            [ d.e.f ]          # same as [d.e.f]
            [ g .  h  . i ]    # same as [g.h.i]
            [ j . "ʞ" . 'l' ]  # same as [j."ʞ".'l']|]
          `shouldBe`
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
          Right (Map.fromList [
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
            Map.fromList [
              "name" .= "Hammer",
              "sku"  .= Integer 738594937],
            Map.empty,
            Map.fromList [
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
          Right (Map.fromList [
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
            `shouldBe` Left "6:3: key error: fruit is already a table"

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
          Right (Map.fromList [
            ("t", table [
                ("a", table [
                    ("q",Integer 3),
                    ("x", table [
                        ("y",Integer 1),
                        ("z",Integer 2)])])])])

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
          `shouldBe` Left "2:4: key error: y is already an array of tables"

  describe "deserialization" deserializationTests
  describe "pretty-printing" prettyTests

tomlString :: Table -> String
tomlString = show . prettyToml

prettyTests :: Spec
prettyTests =
 do it "renders example 1" $
      show (encode (Map.singleton "x" (1 :: Integer)))
        `shouldBe` [quoteStr|
        x = 1|]

    it "renders example 2" $
      fmap tomlString (parse "x=1\ny=2")
        `shouldBe` Right [quoteStr|
        x = 1
        y = 2|]

    it "renders example lists" $
      fmap tomlString (parse "x=[1,'two', [true]]")
        `shouldBe` Right [quoteStr|
        x = [1, "two", [true]]|]

    it "renders empty tables" $
      fmap tomlString (parse "x.y.z={}\nz.y.w=false")
        `shouldBe` Right [quoteStr|
        z.y.w = false

        [x.y.z]|]

    it "renders empty tables in array of tables" $
      fmap tomlString (parse "ex=[{},{},{a=9}]")
        `shouldBe` Right [quoteStr|
        [[ex]]

        [[ex]]

        [[ex]]
        a = 9|]

    it "renders multiple tables" $
      fmap tomlString (parse "a.x=1\nb.x=3\na.y=2\nb.y=4")
        `shouldBe` Right [quoteStr|
        [a]
        x = 1
        y = 2

        [b]
        x = 3
        y = 4|]

    it "renders escapes in strings" $
      fmap tomlString (parse "a=\"\\\\\\b\\t\\r\\n\\f\\\"\\u007f\\U0001000c\"")
        `shouldBe` Right [quoteStr|
        a = "\\\b\t\r\n\f\"\u007F\U0001000C"|]

    it "renders floats" $
      fmap tomlString (parse "a=0.0\nb=-0.1\nc=0.1\nd=3.141592653589793\ne=4e123")
        `shouldBe` Right [quoteStr|
        a = 0.0
        b = -0.1
        c = 0.1
        d = 3.141592653589793
        e = 4.0e123|]

    it "renders special floats" $
      fmap tomlString (parse "a=inf\nb=-inf\nc=nan")
        `shouldBe` Right [quoteStr|
        a = inf
        b = -inf
        c = nan|]

    it "renders empty documents" $
      fmap tomlString (parse "")
        `shouldBe` Right ""

    it "renders dates and time" $
      fmap tomlString (parse [quoteStr|
        a = 2020-05-07
        b = 15:16:17.990
        c = 2020-05-07T15:16:17.990
        d = 2020-05-07T15:16:17.990Z
        e = 2020-05-07T15:16:17-07:00
        f = 2021-09-06T14:15:19+08:00|])
        `shouldBe` Right [quoteStr|
        a = 2020-05-07
        b = 15:16:17.99
        c = 2020-05-07T15:16:17.99
        d = 2020-05-07T15:16:17.99Z
        e = 2020-05-07T15:16:17-07:00
        f = 2021-09-06T14:15:19+08:00|]

    it "renders quoted keys" $
      fmap tomlString (parse "''.'a b'.'\"' = 10")
        `shouldBe` Right [quoteStr|
        ""."a b"."\"" = 10|]

    it "renders inline tables" $
      fmap tomlString (parse [quoteStr|
        x = [[{a = 'this is a longer example', b = 'and it will linewrap'},{c = 'all on its own'}]]|])
        `shouldBe` Right [quoteStr|
          x = [ [ {a = "this is a longer example", b = "and it will linewrap"}
                , {c = "all on its own"} ] ]|]

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

deserializationTests :: Spec
deserializationTests =
     do it "handles fruit example" $
          decode [quoteStr|
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
            Success mempty (Fruits [
                Fruit "apple" (Just (Physical "red" "round")) [Variety "red delicious", Variety "granny smith"],
                Fruit "banana" Nothing [Variety "plantain"]])
