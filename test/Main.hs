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

import Data.Either (isLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import QuoteStr (quoteStr)
import Test.Hspec (hspec, describe, it, shouldBe, shouldSatisfy, Spec)
import Toml (Value(..), parse)

main :: IO ()
main = hspec $
 describe "parse"
 do testCase01
    testCase02
    testCase03
    testCase04
    testCaseDoc08
    testCaseDoc12
    testCaseDoc13

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
          parse "key = # INVALID" `shouldSatisfy` isLeft

        it "requires newlines between assignments" $
          parse "first = \"Tom\" last = \"Preston-Werner\" # INVALID" `shouldSatisfy` isLeft

    describe "keys"
     do it "allows bare keys" $
          parse [quoteStr|
            key = "value"
            bare_key = "value"
            bare-key = "value"
            1234 = "value"|]
          `shouldBe`
          Right (Map.fromList [
            ("1234",String "value"),
            ("bare-key",String "value"),
            ("bare_key",String "value"),
            ("key",String "value")])

        it "allows quoted keys" $
          parse [quoteStr|
            "127.0.0.1" = "value"
            "character encoding" = "value"
            "ʎǝʞ" = "value"
            'key2' = "value"
            'quoted "value"' = "value"|]
          `shouldBe`
          Right (Map.fromList [
            ("127.0.0.1",String "value"),
            ("character encoding",String "value"),
            ("key2",String "value"),
            ("quoted \"value\"",String "value"),
            ("ʎǝʞ",String "value")])

        it "allows dotted keys" $
          parse [quoteStr|
            name = "Orange"
            physical.color = "orange"
            physical.shape = "round"
            site."google.com" = true|]
          `shouldBe`
          Right (Map.fromList [
            ("name",String "Orange"),
            ("physical", table [("color",String "orange"),("shape",String "round")]),
            ("site",table [("google.com",Bool True)])])

        it "prevents duplicate keys" $
          parse [quoteStr|
            name = "Tom"
            name = "Pradyun"|]
          `shouldSatisfy` isLeft

        it "prevents duplicate keys even between bare and quoted" $
          parse [quoteStr|
            spelling = "favorite"
            "spelling" = "favourite"|]
          `shouldSatisfy` isLeft

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
            ("apple", table [
                ("color",String "red"),
                ("skin",String "thin"),
                ("type",String "fruit")]),
            ("orange", table [
                ("color",String "orange"),
                ("skin",String "thick"),
                ("type",String "fruit")])])

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
            ("1900-01-01", Day (read "1900-01-01")),
            ("1_2", Integer 23),
            ("false", Bool False),
            ("true", Bool True)])

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
            ("str1",String "The quick brown fox jumps over the lazy dog."),
            ("str2",String "The quick brown fox jumps over the lazy dog."),
            ("str3",String "The quick brown fox jumps over the lazy dog.")])

        it "allows quotes inside multiline quoted strings" $
          parse [quoteStr|
            str4 = """Here are two quotation marks: "". Simple enough."""
            str5 = """Here are three quotation marks: ""\"."""
            str6 = """Here are fifteen quotation marks: ""\"""\"""\"""\"""\"."""

            # "This," she said, "is just a pointless statement."
            str7 = """"This," she said, "is just a pointless statement.""""|]
          `shouldBe`
          Right (Map.fromList [
            ("str4",String "Here are two quotation marks: \"\". Simple enough."),
            ("str5",String "Here are three quotation marks: \"\"\"."),
            ("str6",String "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"."),
            ("str7",String "\"This,\" she said, \"is just a pointless statement.\"")])

        it "disallows triple quotes inside a multiline string" $
          parse [quoteStr|
            str5 = """Here are three quotation marks: """."""  # INVALID|]
          `shouldSatisfy` isLeft

        it "ignores escapes in literal strings" $
          parse [quoteStr|
            # What you see is what you get.
            winpath  = 'C:\Users\nodejs\templates'
            winpath2 = '\\ServerX\admin$\system32\'
            quoted   = 'Tom "Dubs" Preston-Werner'
            regex    = '<\i\c*\s*>'|]
          `shouldBe`
          Right (Map.fromList [
            ("quoted",String "Tom \"Dubs\" Preston-Werner"),
            ("regex",String "<\\i\\c*\\s*>"),
            ("winpath",String "C:\\Users\\nodejs\\templates"),
            ("winpath2",String "\\\\ServerX\\admin$\\system32\\")])

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
            ("lines",String "The first newline is\ntrimmed in raw strings.\nAll other whitespace\nis preserved.\n"),
            ("regex2",String "I [dw]on't need \\d{2} apples")])

    parsesLiterals

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
                ("colors",Array [String "red",String "yellow",String "green"]),
                ("contributors",Array [
                    String "Foo Bar <foo@example.com>",
                    table [
                        ("email",String "bazqux@example.com"),
                        ("name",String "Baz Qux"),
                        ("url",String "https://example.com/bazqux")]]),
                ("integers", Array [Integer 1,Integer 2,Integer 3]),
                ("nested_arrays_of_ints",Array [Array [Integer 1,Integer 2],Array [Integer 3,Integer 4,Integer 5]]),
                ("nested_mixed_array",Array [Array [Integer 1,Integer 2],Array [String "a",String "b",String "c"]]),
                ("numbers",Array [Float 0.1,Float 0.2,Float 0.5,Integer 1,Integer 2,Integer 5]),
                ("string_array",Array [String "all",String "strings",String "are the same",String "type"])])

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
                ("integers2",Array [Integer 1,Integer 2,Integer 3]),
                ("integers3",Array [Integer 1,Integer 2])])

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
            ("table-1", table [
                ("key1",String "some string"),
                ("key2",Integer 123)]),
            ("table-2", table [
                ("key1",String "another string"),
                ("key2",Integer 456)])])

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
            ("a", table [("b", table [("c", table [])])]),
            ("d", table [("e", table [("f", table [])])]),
            ("g", table [("h", table [("i", table [])])]),
            ("j", table [("ʞ", table [("l", table [])])])])

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
            ("x",table [
                ("q",Integer 1),
                ("y",table [
                    ("z",table [
                        ("w",table [])])])])])

        it "prevents using a [table] to open a table defined with dotted keys" $
          parse [quoteStr|
            [fruit]
            apple.color = 'red'
            apple.taste.sweet = true
            [fruit.apple]|]
          `shouldSatisfy` isLeft
        
        it "can add subtables" $
          parse [quoteStr|
            [fruit]
            apple.color = "red"
            apple.taste.sweet = true
            [fruit.apple.texture]  # you can add sub-tables
            smooth = true|]
          `shouldBe`
          Right (Map.fromList [
            ("fruit", table [
                ("apple", table [
                    ("color",String "red"),
                    ("taste", table [
                        ("sweet",Bool True)]),
                        ("texture", table [
                            ("smooth", Bool True)])])])])

    describe "inline table"
     do it "parses inline tables" $
          parse [quoteStr|
            name = { first = "Tom", last = "Preston-Werner" }
            point = { x = 1, y = 2 }
            animal = { type.name = "pug" }|]
          `shouldBe`
          Right (Map.fromList [
            ("animal",table [("type",table [("name",String "pug")])]),
            ("name",table [("first",String "Tom"),("last",String "Preston-Werner")]),
            ("point",table [("x",Integer 1),("y",Integer 2)])])

        it "prevents altering inline tables with dotted keys" $
          parse [quoteStr|
            [product]
            type = { name = "Nail" }
            type.edible = false  # INVALID|]
          `shouldSatisfy` isLeft

        it "prevents using inline tables to add keys to existing tables" $
          parse [quoteStr|
            [product]
            type.name = "Nail"
            type = { edible = false }  # INVALID|]
          `shouldSatisfy` isLeft

    describe "array of tables"
     do it "supports array of tables syntax" $
          parse [quoteStr|
            [[products]]
            name = "Hammer"
            sku = 738594937

            [[products]]  # empty table within the array

            [[products]]
            name = "Nail"
            sku = 284758393

            color = "gray"|]
          `shouldBe`
          Right (Map.fromList [
            ("products",Array [
                table [
                    ("name",String "Hammer"),
                    ("sku",Integer 738594937)],
                table [],
                table [
                    ("color",String "gray"),
                    ("name",String "Nail"),
                    ("sku",Integer 284758393)]])])

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
            ("fruits",Array [
                table [
                    ("name",String "apple"),
                    ("physical",table [
                        ("color",String "red"),
                        ("shape",String "round")]),
                    ("varieties",Array [
                        table [("name",String "red delicious")],
                        table [("name",String "granny smith")]])],
                table [
                    ("name",String "banana"),
                    ("varieties",Array [
                        table [("name",String "plantain")]])]])])

        it "prevents redefining a supertable with an array of tables" $
          parse [quoteStr|
            # INVALID TOML DOC
            [fruit.physical]  # subtable, but to which parent element should it belong?
            color = "red"
            shape = "round"

            [[fruit]]  # parser must throw an error upon discovering that "fruit" is
                    # an array rather than a table
            name = "apple"|]
            `shouldSatisfy` isLeft

        it "prevents redefining an inline array" $
          parse [quoteStr|
            # INVALID TOML DOC
            fruits = []

            [[fruits]] # Not allowed|]
          `shouldSatisfy` isLeft

goodTestCase :: String -> Map String Value -> Spec
goodTestCase src expect =
    it "starts" $
    parse src `shouldBe` Right expect

badTestCase :: String -> Spec
badTestCase src =
    it "fails" $
    parse src `shouldSatisfy` isLeft

testCase01 :: Spec
testCase01 = goodTestCase
    "x = 123_456_789"
    (Map.fromList [("x", Integer 123456789)])

testCase02 :: Spec
testCase02 = goodTestCase
    "x.y = 0x10\n\
    \x.z = 0o21"
    (Map.fromList [("x", table [("y", Integer 16), ("z", Integer 17)])])

testCase03 :: Spec
testCase03 = goodTestCase
    "[['ok']]\n\
    \[[ok.ko]]\n\
    \m = false"
    (Map.fromList [("ok",Array [table [("ko",Array [table [("m",Bool False)]])]])])

testCase04 :: Spec
testCase04 = badTestCase
    "x.y=1\n\
    \[x]"

parsesLiterals :: Spec
parsesLiterals =
    it "parses literals correctly" $
    parse
    "int1 = +99\n\
    \int2 = 42\n\
    \int3 = 0\n\
    \int4 = -17\n\
    \int5 = 1_000\n\
    \int6 = 5_349_221\n\
    \int7 = 53_49_221  # Indian number system grouping\n\
    \int8 = 1_2_3_4_5  # VALID but discouraged\n\
    \# hexadecimal with prefix `0x`\n\
    \hex1 = 0xDEADBEEF\n\
    \hex2 = 0xdeadbeef\n\
    \hex3 = 0xdead_beef\n\
    \\n\
    \# octal with prefix `0o`\n\
    \oct1 = 0o01234567\n\
    \oct2 = 0o755 # useful for Unix file permissions\n\
    \\n\
    \# binary with prefix `0b`\n\
    \bin1 = 0b11010110\n\
    \# fractional\n\
    \flt1 = +1.0\n\
    \flt2 = 3.1415\n\
    \flt3 = -0.01\n\
    \\n\
    \# exponent\n\
    \flt4 = 5e+22\n\
    \flt5 = 1e06\n\
    \flt6 = -2E-2\n\
    \\n\
    \# both\n\
    \flt7 = 6.626e-34\n\
    \flt8 = 224_617.445_991_228\n\
    \# infinity\n\
    \sf1 = inf  # positive infinity\n\
    \sf2 = +inf # positive infinity\n\
    \sf3 = -inf # negative infinity\n\
    \\n\
    \bool1 = true\n\
    \bool2 = false\n\
    \\n\
    \odt1 = 1979-05-27T07:32:00Z\n\
    \odt2 = 1979-05-27T00:32:00-07:00\n\
    \odt3 = 1979-05-27T00:32:00.999999-07:00\n\
    \odt4 = 1979-05-27 07:32:00Z\n\
    \\n\
    \ldt1 = 1979-05-27T07:32:00\n\
    \ldt2 = 1979-05-27T00:32:00.999999\n\
    \\n\
    \ld1 = 1979-05-27\n\
    \\n\
    \lt1 = 07:32:00\n\
    \lt2 = 00:32:00.999999\n"
    `shouldBe` Right
    (Map.fromList [
        ("bin1",Integer 214),
        ("bool1",Bool True),
        ("bool2",Bool False),
        ("flt1",Float 1.0),
        ("flt2",Float 3.1415),
        ("flt3",Float (-1.0e-2)),
        ("flt4",Float 4.9999999999999996e22),
        ("flt5",Float 1000000.0),
        ("flt6",Float (-2.0e-2)),
        ("flt7",Float 6.626e-34),
        ("flt8",Float 224617.445991228),
        ("hex1",Integer 0xDEADBEEF),
        ("hex2",Integer 0xDEADBEEF),
        ("hex3",Integer 0xDEADBEEF),
        ("int1",Integer 99),
        ("int2",Integer 42),
        ("int3",Integer 0),
        ("int4",Integer (-17)),
        ("int5",Integer 1000),
        ("int6",Integer 5349221),
        ("int7",Integer 5349221),
        ("int8",Integer 12345),
        ("ld1",Day (read "1979-05-27")),
        ("ldt1",LocalTime (read "1979-05-27 07:32:00")),
        ("ldt2",LocalTime (read "1979-05-27 00:32:00.999999")),
        ("lt1",TimeOfDay (read "07:32:00")),
        ("lt2",TimeOfDay (read "00:32:00.999999")),
        ("oct1",Integer 0o01234567),
        ("oct2",Integer 0o755),
        ("odt1",ZonedTime (read "1979-05-27 07:32:00 +0000")),
        ("odt2",ZonedTime (read "1979-05-27 00:32:00 -0700")),
        ("odt3",ZonedTime (read "1979-05-27 00:32:00.999999 -0700")),
        ("odt4",ZonedTime (read "1979-05-27 07:32:00 +0000")),
        ("sf1",Float (1/0)),
        ("sf2",Float (1/0)),
        ("sf3",Float (-1/0))])

testCaseDoc08 :: Spec
testCaseDoc08 =
    it "parses nan correctly"
    do parse
            "# not a number\n\
            \sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific\n\
            \sf5 = +nan # same as `nan`\n\
            \sf6 = -nan # valid, actual encoding is implementation-specific\n"
        `shouldSatisfy` \case
          Left{} -> False
          Right x -> all checkNaN x
    where
        checkNaN (Float x) = isNaN x
        checkNaN _         = False

testCaseDoc12 :: Spec
testCaseDoc12 =
    it "allows defining subtables of array tables" $
    parse [quoteStr|
        [[x]]
        a=1
        [x.y.z]
        b=2
        [x.y]
        c=3|]
    `shouldBe`
    Right (Map.fromList [
        ("x",Array [
            table ([
                ("a",Integer 1),
                ("y",table [
                    ("c",Integer 3),
                    ("z",table [
                        ("b",Integer 2)])])])])])

-- Likewise, using dotted keys to redefine tables already defined in [table] form is not allowed.
testCaseDoc13 :: Spec
testCaseDoc13 =
    it "prevents using dotted keys to redefine tables already defined in [table] form" $
    parse [quoteStr|
        [x.y]
        z.w=1
        [x]
        y.q=2|]
    `shouldSatisfy` isLeft

table :: [(String, Value)] -> Value
table = Table . Map.fromList
