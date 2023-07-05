{-# Language DuplicateRecordFields #-}
module DecodeSpec (spec) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import QuoteStr (quoteStr)
import Test.Hspec (it, shouldBe, Spec)
import Toml (decode, Result(Success), encode)
import Toml.FromValue (FromTable(..), FromValue(..), defaultTableFromValue, runParseTable, reqKey, optKey)
import Toml.FromValue.Generic (genericFromTable)
import Toml.ToValue (ToTable(..), ToValue(toValue), (.=), defaultTableToValue)
import Toml.ToValue.Generic (genericToTable)
import Toml (Result(..))

newtype Fruits = Fruits { fruits :: [Fruit] }
    deriving (Eq, Show, Generic)

data Fruit = Fruit {
    name :: String,
    physical :: Maybe Physical,
    varieties :: [Variety]
    } deriving (Eq, Show, Generic)

data Physical = Physical {
    color :: String,
    shape :: String
    } deriving (Eq, Show, Generic)

newtype Variety = Variety {
    name :: String
    } deriving (Eq, Show, Generic)

instance FromTable Fruits   where fromTable = genericFromTable
instance FromTable Physical where fromTable = genericFromTable
instance FromTable Variety  where fromTable = genericFromTable

instance FromValue Fruits   where fromValue = defaultTableFromValue
instance FromValue Fruit    where fromValue = defaultTableFromValue
instance FromValue Physical where fromValue = defaultTableFromValue
instance FromValue Variety  where fromValue = defaultTableFromValue

instance ToTable Fruits   where toTable = genericToTable
instance ToTable Physical where toTable = genericToTable
instance ToTable Variety  where toTable = genericToTable

instance ToValue Fruits   where toValue = defaultTableToValue
instance ToValue Fruit    where toValue = defaultTableToValue
instance ToValue Physical where toValue = defaultTableToValue
instance ToValue Variety  where toValue = defaultTableToValue

instance FromTable Fruit where
    fromTable = runParseTable (Fruit
        <$> reqKey "name"
        <*> optKey "physical"
        <*> (fromMaybe [] <$> optKey "varieties"))

instance ToTable Fruit where
    toTable (Fruit n mbp vs) = Map.fromList $
        ["varieties" .= vs | not (null vs)] ++
        ["physical"  .= p | Just p <- [mbp]] ++
        ["name"      .= n]

spec :: Spec
spec =
 do let expect = Fruits [
            Fruit "apple" (Just (Physical "red" "round")) [Variety "red delicious", Variety "granny smith"],
            Fruit "banana" Nothing [Variety "plantain"]]

    it "handles fruit example" $
        decode [quoteStr|
            [[fruits]]
            name = "apple"

            [fruits.physical]  # subtable
            color = "red"
            shape = "round"

            [[fruits.varieties]]  # nested array of tables
            name = "red delicious"

            [[fruits.varieties]]
            name = "granny smith"

            [[fruits]]
            name = "banana"

            [[fruits.varieties]]
            name = "plantain"|]
        `shouldBe`
        Success mempty expect

    it "encodes correctly" $
        show (encode expect)
        `shouldBe`
        [quoteStr|
            [[fruits]]
            name = "apple"

            [fruits.physical]
            color = "red"
            shape = "round"

            [[fruits.varieties]]
            name = "red delicious"

            [[fruits.varieties]]
            name = "granny smith"

            [[fruits]]
            name = "banana"

            [[fruits.varieties]]
            name = "plantain"|]

    it "generates warnings for unused keys" $
        decode [quoteStr|
            [[fruits]]
            name = "peach"
            taste = "sweet"
            count = 5
            [[fruits]]
            name = "pineapple"
            color = "yellow"|]
        `shouldBe`
        Success [
            "Unexpected keys: count, taste in top.fruits[0]",
            "Unexpected key: color in top.fruits[1]"]
            (Fruits [Fruit "peach" Nothing [], Fruit "pineapple" Nothing []])
    
    it "handles missing key errors" $
        (decode "[[fruits]]" :: Result Fruits)
        `shouldBe`
        Failure ["missing key: name in top.fruits[0]"]

    it "handles parse errors while decoding" $
        (decode "x =" :: Result Fruits)
        `shouldBe`
        Failure ["1:4: parse error: unexpected end-of-input"]
