{-|
Module      : FromValueSpec
Description : Exercise various components of FromValue
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module FromValueSpec (spec) where

import Control.Applicative ((<|>), empty)
import Control.Monad (when)
import Test.Hspec (it, shouldBe, Spec)
import Toml
import Toml.FromValue (FromValue(fromValue), optKey, reqKey, warnTable, pickKey, runParseTable)
import Toml.FromValue.Matcher (Matcher, runMatcher)
import Toml.FromValue.ParseTable (KeyAlt(..))
import Toml.Pretty (prettyMatchMessage)
import Toml.ToValue (table, (.=))

humanMatcher :: Matcher l a -> Result String a
humanMatcher m =
    case runMatcher m of
        Failure e -> Failure (prettyMatchMessage . fmap (const "testcase") <$> e)
        Success w x -> Success (prettyMatchMessage . fmap (const "testcase") <$> w) x

spec :: Spec
spec =
 do it "handles one reqKey" $
        humanMatcher (runParseTable (reqKey "test") () (table ["test" .= "val"]))
        `shouldBe`
        Success [] "val"

    it "handles one optKey" $
        humanMatcher (runParseTable (optKey "test") () (table ["test" .= "val"]))
        `shouldBe`
        Success [] (Just "val")

    it "handles one missing optKey" $
        humanMatcher (runParseTable (optKey "test") () (table ["nottest" .= "val"]))
        `shouldBe`
        Success ["testcase: unexpected key: nottest in top"] (Nothing :: Maybe String)

    it "handles one missing reqKey" $
        humanMatcher (runParseTable (reqKey "test") () (table ["nottest" .= "val"]))
        `shouldBe`
        (Failure ["testcase: missing key: test in top"] :: Result String String)

    it "handles one mismatched reqKey" $
        humanMatcher (runParseTable (reqKey "test") () (table ["test" .= "val"]))
        `shouldBe`
        (Failure ["testcase: type error. wanted: integer got: string in top.test"] :: Result String Integer)

    it "handles one mismatched optKey" $
        humanMatcher (runParseTable (optKey "test") () (table ["test" .= "val"]))
        `shouldBe`
        (Failure ["testcase: type error. wanted: integer got: string in top.test"] :: Result String (Maybe Integer))

    it "handles concurrent errors" $
        humanMatcher (runParseTable (reqKey "a" <|> empty <|> reqKey "b") () (table []))
        `shouldBe`
        (Failure ["testcase: missing key: a in top",
                  "testcase: missing key: b in top"] :: Result String Integer)

    it "handles concurrent value mismatch" $
        let v = String "" in
        humanMatcher (Left <$> fromValue v <|> empty <|> Right <$> fromValue v)
        `shouldBe`
        (Failure [
            "testcase: type error. wanted: boolean got: string in top",
            "testcase: type error. wanted: integer got: string in top"]
            :: Result String (Either Bool Int))

    it "doesn't emit an error for empty" $
        humanMatcher (runParseTable empty () (table []))
        `shouldBe`
        (Failure [] :: Result String Integer)

    it "matches single characters" $
        runMatcher (fromValue (String "x"))
        `shouldBe`
        Success [] 'x'

    it "rejections non-single characters" $
        humanMatcher (fromValue (String "xy"))
        `shouldBe`
        (Failure ["testcase: type error. wanted: character got: string in top"] :: Result String Char)

    it "collects warnings in table matching" $
        let pt =
             do i1 <- reqKey "k1"
                i2 <- reqKey "k2"
                let n = i1 + i2
                when (odd n) (warnTable "k1 and k2 sum to an odd value")
                pure n
        in
        humanMatcher (runParseTable pt () (table ["k1" .= (1 :: Integer), "k2" .= (2 :: Integer)]))
        `shouldBe`
        Success ["k1 and k2 sum to an odd value in top"] (3 :: Integer)

    it "offers helpful messages when no keys match" $
        let pt = pickKey [Key "this" \_ -> pure 'a', Key "." \_ -> pure 'b']
        in
        humanMatcher (runParseTable pt () (MkTable mempty))
        `shouldBe`
        (Failure ["testcase: possible keys: this, \".\" in top"] :: Result String Char)

    it "generates an error message on an empty pickKey" $
        let pt = pickKey []
        in
        humanMatcher (runParseTable pt () (MkTable mempty))
        `shouldBe`
        (Failure [] :: Result String Char)
