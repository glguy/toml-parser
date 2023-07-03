module GenericSpec (spec) where

import GHC.Generics (Generic)
import QuoteStr (quoteStr)
import Test.Hspec (it, shouldBe, Spec)
import Toml (decode, encode, Result(Success))
import Toml.FromValue (FromTable(..), defaultTableFromValue, FromValue(fromValue))
import Toml.FromValue.Generic ( genericFromTable )
import Toml.ToValue (ToTable(..), defaultTableToValue, ToValue(toValue))
import Toml.ToValue.Generic (genericToTable)

data ExampleRecord = ExampleRecord {
  exString :: String,
  exList   :: [Int],
  exOpt    :: Maybe Bool}
  deriving (Show, Generic, Eq)

instance FromTable ExampleRecord where fromTable = genericFromTable
instance FromValue ExampleRecord where fromValue = defaultTableFromValue
instance ToTable   ExampleRecord where toTable   = genericToTable
instance ToValue   ExampleRecord where toValue   = defaultTableToValue

spec :: Spec
spec =
 do let ex1 = ExampleRecord "one" [2,3] (Just True)

    it "encodes correctly" $
      show (encode ex1)
      `shouldBe`
      [quoteStr|
        exList = [2, 3]
        exOpt = true
        exString = "one"|]

    it "decodes correctly" $
      decode (show (encode ex1))
      `shouldBe`
      Success [] ex1

    let ex2 = ExampleRecord "two" [] Nothing

    it "encodes correctly with missing optional" $
      show (encode ex2)
      `shouldBe`
      [quoteStr|
        exList = []
        exString = "two"|]

    it "decodes correctly again" $
      decode (show (encode ex2))
      `shouldBe`
      Success [] ex2
