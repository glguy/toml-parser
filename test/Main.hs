module Main (main) where

import Data.Map qualified as Map
import Data.Map (Map)
import System.Exit (exitFailure)
import Toml
import Control.Monad (unless)

main :: IO ()
main =
 do testCase01
    testCase02
    testCase03
    testCase04
    testCaseDoc01
    testCaseDoc02
    testCaseDoc03
    testCaseDoc04
    testCaseDoc05
    testCaseDoc06
    testCaseDoc07
    testCaseDoc08
    testCaseDoc09
    testCaseDoc10
    testCaseDoc11

goodTestCase :: String -> Map String Value -> IO ()
goodTestCase src expect =
    case parse src of
        Right t
            | t == expect -> pure ()
            | otherwise   ->
             do putStrLn ("Parsed:   " ++ src        )
                putStrLn ("Got:      " ++ show t     )
                putStrLn ("Expected: " ++ show expect)
                exitFailure
        Left e ->
             do putStrLn ("Parsed:   " ++ src        )
                putStrLn ("Error:    " ++ e          )
                exitFailure

badTestCase :: String -> IO ()
badTestCase src =
    case parse src of
        Right t ->
             do putStrLn ("Parsed:   " ++ src        )
                putStrLn ("Got:      " ++ show t     )
                putStrLn "Expected failure"
        Left{} -> pure ()

testCase01 :: IO ()
testCase01 = goodTestCase
    "x = 123_456_789"
    (Map.fromList [("x", Integer 123456789)])

testCase02 :: IO ()
testCase02 = goodTestCase
    "x.y = 0x10\n\
    \x.z = 0o21"
    (Map.fromList [("x", table [("y", Integer 16), ("z", Integer 17)])])

testCase03 :: IO ()
testCase03 = goodTestCase
    "[['ok']]\n\
    \[[ok.ko]]\n\
    \m = false"
    (Map.fromList [("ok",Array [table [("ko",Array [table [("m",Bool False)]])]])])

testCase04 :: IO ()
testCase04 = badTestCase
    "x.y=1\n\
    \[x]"

testCaseDoc01 :: IO ()
testCaseDoc01 = goodTestCase
    "# This is a full-line comment\n\
    \key = \"value\"  # This is a comment at the end of a line\n\
    \another = \"# This is not a comment\""
    (Map.fromList [("another",String "# This is not a comment"),("key",String "value")])

testCaseDoc02 :: IO ()
testCaseDoc02 = badTestCase
    "key = # INVALID"

testCaseDoc03 :: IO ()
testCaseDoc03 = badTestCase "first = \"Tom\" last = \"Preston-Werner\" # INVALID"

testCaseDoc04 :: IO ()
testCaseDoc04 = goodTestCase
    "key = \"value\"\n\
    \bare_key = \"value\"\n\
    \bare-key = \"value\"\n\
    \1234 = \"value\""
    (Map.fromList [("1234",String "value"),("bare-key",String "value"),("bare_key",String "value"),("key",String "value")])

testCaseDoc05 :: IO ()
testCaseDoc05 = goodTestCase
    "\"127.0.0.1\" = \"value\"\n\
    \\"character encoding\" = \"value\"\n\
    \\"ʎǝʞ\" = \"value\"\n\
    \'key2' = \"value\"\n\
    \'quoted \"value\"' = \"value\""
    (Map.fromList [("127.0.0.1",String "value"),("character encoding",String "value"),("key2",String "value"),("quoted \"value\"",String "value"),("ʎǝʞ",String "value")])

testCaseDoc06 :: IO ()
testCaseDoc06 = goodTestCase
    "name = \"Orange\"\n\
    \physical.color = \"orange\"\n\
    \physical.shape = \"round\"\n\
    \site.\"google.com\" = true\n"
    (Map.fromList [("name",String "Orange"),("physical", table [("color",String "orange"),("shape",String "round")]),("site",table [("google.com",Bool True)])])

testCaseDoc07 :: IO ()
testCaseDoc07 = goodTestCase
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
    \lt2 = 00:32:00.999999\n\
    \# arrays\n\
    \integers = [ 1, 2, 3 ]\n\
    \colors = [ \"red\", \"yellow\", \"green\" ]\n\
    \nested_arrays_of_ints = [ [ 1, 2 ], [3, 4, 5] ]\n\
    \nested_mixed_array = [ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]\n\
    \string_array = [ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''' ]\n"

    (Map.fromList [
        ("bin1",Integer 214),
        ("bool1",Bool True),
        ("bool2",Bool False),
        ("colors",Array [String "red",String "yellow",String "green"]),
        ("flt1",Float 1.0),
        ("flt2",Float 3.1415),
        ("flt3",Float (-1.0e-2)),
        ("flt4",Float 4.9999999999999996e22),
        ("flt5",Float 1000000.0),
        ("flt6",Float (-2.0e-2)),
        ("flt7",Float 6.626e-34),
        ("flt8",Float 224617.445991228),
        ("hex1",Integer 3735928559),
        ("hex2",Integer 3735928559),
        ("hex3",Integer 3735928559),
        ("int1",Integer 99),
        ("int2",Integer 42),
        ("int3",Integer 0),
        ("int4",Integer (-17)),
        ("int5",Integer 1000),
        ("int6",Integer 5349221),
        ("int7",Integer 5349221),
        ("int8",Integer 12345),
        ("integers",Array [Integer 1,Integer 2,Integer 3]),
        ("ld1",Day (read "1979-05-27")),
        ("ldt1",LocalTime (read "1979-05-27 07:32:00")),
        ("ldt2",LocalTime (read "1979-05-27 00:32:00.999999")),
        ("lt1",TimeOfDay (read "07:32:00")),
        ("lt2",TimeOfDay (read "00:32:00.999999")),
        ("nested_arrays_of_ints",
        Array [Array [Integer 1,Integer 2],Array [Integer 3,Integer 4,Integer 5]]),
        ("nested_mixed_array",Array [Array [Integer 1,Integer 2],Array [String "a",String "b",String "c"]]),
        ("oct1",Integer 342391),
        ("oct2",Integer 493),
        ("odt1",ZonedTime (read "1979-05-27 07:32:00 +0000")),
        ("odt2",ZonedTime (read "1979-05-27 00:32:00 -0700")),
        ("odt3",ZonedTime (read "1979-05-27 00:32:00.999999 -0700")),
        ("odt4",ZonedTime (read "1979-05-27 07:32:00 +0000")),
        ("sf1",Float (1/0)),
        ("sf2",Float (1/0)),
        ("sf3",Float (-1/0)),
        ("string_array",Array [String "all",String "strings",String "are the same",String "type"])])

testCaseDoc08 :: IO ()
testCaseDoc08 =
    case parse txt of
        Left{} -> putStrLn "nan case failed" >> exitFailure
        Right t -> unless (all nanCheck t) (putStrLn "nan case failed" >> exitFailure)
    where
        nanCheck (Float n) = isNaN n
        nanCheck _ = False

        txt =
            "# not a number\n\
            \sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific\n\
            \sf5 = +nan # same as `nan`\n\
            \sf6 = -nan # valid, actual encoding is implementation-specific\n"

testCaseDoc09 :: IO ()
testCaseDoc09 = goodTestCase
    "[fruit]\n\
    \apple.color = 'red'\n\
    \apple.taste.sweet = true\n\
    \\n\
    \[fruit.apple.texture]  # you can add sub-tables\n\
    \smooth = true\n"
    (Map.fromList [
        ("fruit", table [
            ("apple",table [
                ("color",String "red"),
                ("taste",table [
                    ("sweet",Bool True)]),
                ("texture",table [
                    ("smooth",Bool True)])])])])

testCaseDoc10 :: IO ()
testCaseDoc10 = goodTestCase
    "[x.y.z.w] # for this to work\n\
    \[x] # defining a super-table afterward is ok\n"
    (Map.fromList [
        ("x",table [
            ("y",table [
                ("z",table [
                    ("w",table [])])])])])

testCaseDoc11 :: IO ()
testCaseDoc11 = badTestCase
    "[fruit]\n\
    \apple.color = 'red'\n\
    \apple.taste.sweet = true\n\
    \\n\
    \[fruit.apple]"

table :: [(String, Value)] -> Value
table = Table . Map.fromList