{-# Language OverloadedStrings #-}
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
import Toml.Schema
import Toml.Syntax (startPos)

humanMatcher :: Matcher l a -> Result String a
humanMatcher m =
    case runMatcher m of
        Failure e -> Failure (prettyMatchMessage . fmap (const startPos) <$> e)
        Success w x -> Success (prettyMatchMessage . fmap (const startPos) <$> w) x

spec :: Spec
spec =
 do it "handles one reqKey" $
        humanMatcher (parseTable (reqKey "test") () (table ["test" .= Text "val"]))
        `shouldBe`
        Success [] ("val" :: String)

    it "handles one optKey" $
        humanMatcher (parseTable (optKey "test") () (table ["test" .= Text "val"]))
        `shouldBe`
        Success [] (Just ("val" :: String))

    it "handles one missing optKey" $
        humanMatcher (parseTable (optKey "test") () (table ["nottest" .= Text "val"]))
        `shouldBe`
        Success ["1:1: unexpected key: nottest in <top-level>"] (Nothing :: Maybe String)

    it "handles one missing reqKey" $
        humanMatcher (parseTable (reqKey "test") () (table ["nottest" .= Text "val"]))
        `shouldBe`
        (Failure ["1:1: missing key: test in <top-level>"] :: Result String String)

    it "handles one mismatched reqKey" $
        humanMatcher (parseTable (reqKey "test") () (table ["test" .= Text "val"]))
        `shouldBe`
        (Failure ["1:1: expected integer but got string in test"] :: Result String Integer)

    it "handles one mismatched optKey" $
        humanMatcher (parseTable (optKey "test") () (table ["test" .= Text "val"]))
        `shouldBe`
        (Failure ["1:1: expected integer but got string in test"] :: Result String (Maybe Integer))

    it "handles concurrent errors" $
        humanMatcher (parseTable (reqKey "a" <|> empty <|> reqKey "b") () (table []))
        `shouldBe`
        (Failure ["1:1: missing key: a in <top-level>",
                  "1:1: missing key: b in <top-level>"] :: Result String Integer)

    it "handles concurrent value mismatch" $
        let v = "" in
        humanMatcher (Left <$> fromValue v <|> empty <|> Right <$> fromValue v)
        `shouldBe`
        (Failure [
            "1:1: expected boolean but got string in <top-level>",
            "1:1: expected integer but got string in <top-level>"]
            :: Result String (Either Bool Int))

    it "doesn't emit an error for empty" $
        humanMatcher (parseTable empty () (table []))
        `shouldBe`
        (Failure [] :: Result String Integer)

    it "matches single characters" $
        runMatcher (fromValue (Text "x"))
        `shouldBe`
        Success [] 'x'

    it "rejections non-single characters" $
        humanMatcher (fromValue (Text "xy"))
        `shouldBe`
        (Failure ["1:1: expected single character in <top-level>"] :: Result String Char)

    it "collects warnings in table matching" $
        let pt =
             do i1 <- reqKey "k1"
                i2 <- reqKey "k2"
                let n = i1 + i2
                when (odd n) (warnTable "k1 and k2 sum to an odd value")
                pure n
        in
        humanMatcher (parseTable pt () (table ["k1" .= (1 :: Integer), "k2" .= (2 :: Integer)]))
        `shouldBe`
        Success ["k1 and k2 sum to an odd value in <top-level>"] (3 :: Integer)

    it "offers helpful messages when no keys match" $
        let pt = pickKey [Key "this" \_ -> pure 'a', Key "." \_ -> pure 'b']
        in
        humanMatcher (parseTable pt () (table []))
        `shouldBe`
        (Failure ["1:1: possible keys: this, \".\" in <top-level>"] :: Result String Char)

    it "generates an error message on an empty pickKey" $
        let pt = pickKey []
        in
        humanMatcher (parseTable pt () (table []))
        `shouldBe`
        (Failure [] :: Result String Char)
