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
import Toml (Result(..), Value(..))
import Toml.FromValue (Result(..), FromValue(fromValue), optKey, parseTableFromValue, reqKey, warnTable, pickKey)
import Toml.FromValue.Matcher (Matcher, runMatcher)
import Toml.FromValue.ParseTable (KeyAlt(..))
import Toml.Pretty (prettyMatchMessage)
import Toml.ToValue (table)

humanMatcher :: Matcher a -> Result String a
humanMatcher m =
    case runMatcher m of
        Failure e -> Failure (prettyMatchMessage <$> e)
        Success w x -> Success (prettyMatchMessage <$> w) x

spec :: Spec
spec =
 do it "handles one reqKey" $
        humanMatcher (parseTableFromValue (reqKey "test") (table [("test", String "val")]))
        `shouldBe`
        Success [] "val"

    it "handles one optKey" $
        humanMatcher (parseTableFromValue (optKey "test") (table [("test", String "val")]))
        `shouldBe`
        Success [] (Just "val")

    it "handles one missing optKey" $
        humanMatcher (parseTableFromValue (optKey "test") (table [("nottest", String "val")]))
        `shouldBe`
        Success ["unexpected key: nottest in top"] (Nothing :: Maybe String)

    it "handles one missing reqKey" $
        humanMatcher (parseTableFromValue (reqKey "test") (table [("nottest", String "val")]))
        `shouldBe`
        (Failure ["missing key: test in top"] :: Result String String)

    it "handles one mismatched reqKey" $
        humanMatcher (parseTableFromValue (reqKey "test") (table [("test", String "val")]))
        `shouldBe`
        (Failure ["type error. wanted: integer got: string in top.test"] :: Result String Integer)

    it "handles one mismatched optKey" $
        humanMatcher (parseTableFromValue (optKey "test") (table [("test", String "val")]))
        `shouldBe`
        (Failure ["type error. wanted: integer got: string in top.test"] :: Result String (Maybe Integer))

    it "handles concurrent errors" $
        humanMatcher (parseTableFromValue (reqKey "a" <|> empty <|> reqKey "b") (table []))
        `shouldBe`
        (Failure ["missing key: a in top", "missing key: b in top"] :: Result String Integer)

    it "handles concurrent value mismatch" $
        let v = String "" in
        humanMatcher (Left <$> fromValue v <|> empty <|> Right <$> fromValue v)
        `shouldBe`
        (Failure [
            "type error. wanted: boolean got: string in top",
            "type error. wanted: integer got: string in top"]
            :: Result String (Either Bool Int))

    it "doesn't emit an error for empty" $
        humanMatcher (parseTableFromValue empty (table []))
        `shouldBe`
        (Failure [] :: Result String Integer)

    it "matches single characters" $
        runMatcher (fromValue (String "x"))
        `shouldBe`
        Success [] 'x'

    it "rejections non-single characters" $
        humanMatcher (fromValue (String "xy"))
        `shouldBe`
        (Failure ["type error. wanted: character got: string in top"] :: Result String Char)

    it "collects warnings in table matching" $
        let pt =
             do i1 <- reqKey "k1"
                i2 <- reqKey "k2"
                let n = i1 + i2
                when (odd n) (warnTable "k1 and k2 sum to an odd value")
                pure n
        in
        humanMatcher (parseTableFromValue pt (table [("k1", Integer 1), ("k2", Integer 2)]))
        `shouldBe`
        Success ["k1 and k2 sum to an odd value in top"] (3 :: Integer)

    it "offers helpful messages when no keys match" $
        let pt = pickKey [Key "this" \_ -> pure 'a', Key "." \_ -> pure 'b']
        in
        humanMatcher (parseTableFromValue pt (table []))
        `shouldBe`
        (Failure ["possible keys: this, \".\" in top"] :: Result String Char)

    it "generates an error message on an empty pickKey" $
        let pt = pickKey []
        in
        humanMatcher (parseTableFromValue pt (table []))
        `shouldBe`
        (Failure [] :: Result String Char)
