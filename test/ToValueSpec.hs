module ToValueSpec where

import Test.Hspec (it, shouldBe, Spec)
import Toml
import Toml.ToValue (ToValue(toValue))

spec :: Spec
spec =
 do it "converts characters as singleton strings" $
        toValue '!' `shouldBe` String "!"

    it "converts strings normally" $
        toValue "demo" `shouldBe` String "demo"

    it "converts lists" $
        toValue [1,2,3::Int] `shouldBe` Array [Integer 1, Integer 2, Integer 3]
