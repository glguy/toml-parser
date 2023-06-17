module Main (main) where

import Data.Map qualified as Map
import Data.Map (Map)
import System.Exit (exitFailure)
import Toml

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

table :: [(String, Value)] -> Value
table = Table . Map.fromList