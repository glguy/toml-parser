{-# Language OverloadedStrings #-}
module ToValueSpec where

import Test.Hspec (it, shouldBe, Spec)
import Toml
import Toml.ToValue (ToValue(toValue))

spec :: Spec
spec =
 do it "converts characters as singleton strings" $
        toValue '!' `shouldBe` Text "!"

    it "converts strings normally" $
        toValue ("demo" :: String) `shouldBe` Text "demo"

    it "converts lists" $
        toValue [1,2,3::Int] `shouldBe` List [Integer 1, Integer 2, Integer 3]
