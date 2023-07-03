module DecodeSpec (spec) where

import QuoteStr (quoteStr )
import Test.Hspec (it, shouldBe, Spec)
import Toml (decode, Result(Success))
import Toml.FromValue (FromTable(..), FromValue(..), defaultTableFromValue, runParseTable, reqKey, optKey)

newtype Fruits = Fruits [Fruit]
    deriving (Eq, Show)

data Fruit = Fruit String (Maybe Physical) [Variety]
    deriving (Eq, Show)

data Physical = Physical String String
    deriving (Eq, Show)

newtype Variety = Variety String
    deriving (Eq, Show)

instance FromTable Fruits where
    fromTable = runParseTable (Fruits <$> reqKey "fruits")

instance FromTable Fruit where
    fromTable = runParseTable (Fruit <$> reqKey "name" <*> optKey "physical" <*> reqKey "varieties")

instance FromTable Physical where
    fromTable = runParseTable (Physical <$> reqKey "color" <*> reqKey "shape")

instance FromTable Variety where
    fromTable = runParseTable (Variety <$> reqKey "name")

instance FromValue Fruits   where fromValue = defaultTableFromValue
instance FromValue Fruit    where fromValue = defaultTableFromValue
instance FromValue Physical where fromValue = defaultTableFromValue
instance FromValue Variety  where fromValue = defaultTableFromValue

spec :: Spec
spec =
 do it "handles fruit example" $
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
        Success mempty (Fruits [
            Fruit "apple" (Just (Physical "red" "round")) [Variety "red delicious", Variety "granny smith"],
            Fruit "banana" Nothing [Variety "plantain"]])
