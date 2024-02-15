{-# Language DuplicateRecordFields #-}
{-|
Module      : DecodeSpec
Description : Show that decoding TOML works using the various provided classes
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

-}
module DecodeSpec (spec) where

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import QuoteStr (quoteStr)
import Test.Hspec (it, shouldBe, Spec)
import Toml (decode, Result, encode)
import Toml.FromValue (FromValue(..), reqKey, optKey)
import Toml.FromValue.Generic (genericParseTable)
import Toml.ToValue (ToTable(..), ToValue(toValue), table, (.=), defaultTableToValue)
import Toml.ToValue.Generic (genericToTable)
import Toml (Result(..))
import Toml.FromValue (parseTableFromValue)

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

instance FromValue Fruits   where fromValue = parseTableFromValue genericParseTable
instance FromValue Physical where fromValue = parseTableFromValue genericParseTable
instance FromValue Variety  where fromValue = parseTableFromValue genericParseTable

instance ToTable Fruits   where toTable = genericToTable
instance ToTable Physical where toTable = genericToTable
instance ToTable Variety  where toTable = genericToTable

instance ToValue Fruits   where toValue = defaultTableToValue
instance ToValue Fruit    where toValue = defaultTableToValue
instance ToValue Physical where toValue = defaultTableToValue
instance ToValue Variety  where toValue = defaultTableToValue

instance FromValue Fruit where
    fromValue = parseTableFromValue (Fruit
        <$> reqKey "name"
        <*> optKey "physical"
        <*> (fromMaybe [] <$> optKey "varieties"))

instance ToTable Fruit where
    toTable (Fruit n mbp vs) = table $
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
            "4:1: unexpected key: count in fruits[0]",
            "3:1: unexpected key: taste in fruits[0]",
            "7:1: unexpected key: color in fruits[1]"]
            (Fruits [Fruit "peach" Nothing [], Fruit "pineapple" Nothing []])

    it "handles missing key errors" $
        (decode "[[fruits]]" :: Result String Fruits)
        `shouldBe`
        Failure ["1:3: missing key: name in fruits[0]"]

    it "handles parse errors while decoding" $
        (decode "x =" :: Result String Fruits)
        `shouldBe`
        Failure ["1:4: parse error: unexpected end-of-input"]
