module PrettySpec (spec) where

import Test.Hspec (it, shouldBe, Spec)
import QuoteStr (quoteStr)
import Toml (encode, parse, prettyToml, Table)
import Data.Map qualified as Map

tomlString :: Table -> String
tomlString = show . prettyToml

spec :: Spec
spec =
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
            [x.y.z]

            [z]
            y.w = false|]

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

    it "factors out unique table prefixes in leaf tables" $
        fmap tomlString (parse [quoteStr|
            [x]
            i = 1
            p.q = "a"
            y.z = "c"|])
        `shouldBe` Right [quoteStr|
            [x]
            i = 1
            p.q = "a"
            y.z = "c"|]
