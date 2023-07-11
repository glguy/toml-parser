{-|
Module      : FromValueSpec
Description : Exercise various components of FromValue
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module FromValueSpec (spec) where

import Test.Hspec (it, shouldBe, Spec)
import Toml (Result(..), Value(String))
import Toml.FromValue (Result(..), FromValue(fromValue), optKey, parseTableFromValue, reqKey)
import Toml.FromValue.Matcher (runMatcher)
import Toml.ToValue (table)
import Control.Applicative ((<|>), empty)

spec :: Spec
spec =
 do it "handles one reqKey" $
        runMatcher (parseTableFromValue (reqKey "test") (table [("test", String "val")]))
        `shouldBe`
        Success [] "val"

    it "handles one optKey" $
        runMatcher (parseTableFromValue (optKey "test") (table [("test", String "val")]))
        `shouldBe`
        Success [] (Just "val")

    it "handles one missing optKey" $
        runMatcher (parseTableFromValue (optKey "test") (table [("nottest", String "val")]))
        `shouldBe`
        Success ["unexpected key: nottest in top"] (Nothing :: Maybe String)

    it "handles one missing reqKey" $
        runMatcher (parseTableFromValue (reqKey "test") (table [("nottest", String "val")]))
        `shouldBe`
        (Failure ["missing key: test in top"] :: Result String)

    it "handles one mismatched reqKey" $
        runMatcher (parseTableFromValue (reqKey "test") (table [("test", String "val")]))
        `shouldBe`
        (Failure ["type error. wanted: integer got: string in top.test"] :: Result Integer)

    it "handles one mismatched optKey" $
        runMatcher (parseTableFromValue (optKey "test") (table [("test", String "val")]))
        `shouldBe`
        (Failure ["type error. wanted: integer got: string in top.test"] :: Result (Maybe Integer))

    it "handles concurrent errors" $
        runMatcher (parseTableFromValue (reqKey "a" <|> reqKey "b") (table []))
        `shouldBe`
        (Failure ["missing key: a in top", "missing key: b in top"] :: Result Integer)
    
    it "handles concurrent value mismatch" $
        let v = String "" in
        runMatcher (Left <$> fromValue v <|> Right <$> fromValue v)
        `shouldBe`
        (Failure [
            "type error. wanted: boolean got: string in top",
            "type error. wanted: integer got: string in top"]
            :: Result (Either Bool Int))

    it "doesn't emit an error for empty" $
        runMatcher (parseTableFromValue empty (table []))
        `shouldBe`
        (Failure [] :: Result Integer)
