{-# Language GADTs, OverloadedStrings #-}
{-|
Module      : HieDemoSpec
Description : Exercise various components of FromValue on a life-sized example
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module demonstrates how "Toml.FromValue" can handle a real-world
format as used in hie-bios. These types are copied from
<https://github.com/haskell/hie-bios/blob/master/src/HIE/Bios/Config/YAML.hs>
with slight alterations because the Other case is for YAML-specific extensibility.
This approach would work just the same when parameterized in that same way.

-}
module HieDemoSpec where

import Data.Text (Text)
import GHC.Generics ( Generic )
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (decode)
import Toml.Schema as Toml

-----------------------------------------------------------------------
-- THIS CODE DERIVED FROM CODE UNDER THE FOLLOWING LICENSE
-----------------------------------------------------------------------

-- Copyright (c) 2009, IIJ Innovation Institute Inc.
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:

--   * Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--   * Neither the name of the copyright holders nor the names of its
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

data CradleConfig = CradleConfig
    { cradle       :: CradleComponent
    , dependencies :: Maybe [FilePath]
    } deriving (Generic, Show, Eq)

data CradleComponent
    = Multi [MultiSubComponent]
    | Cabal CabalConfig
    | Stack StackConfig
    | Direct DirectConfig
    | Bios BiosConfig
    | None NoneConfig
    deriving (Generic, Show, Eq)

data NoneConfig = NoneConfig
  deriving (Generic, Show, Eq)

data MultiSubComponent = MultiSubComponent
    { path   :: FilePath
    , config :: CradleConfig
    } deriving (Generic, Show, Eq)

data CabalConfig = CabalConfig
    { cabalProject    :: Maybe FilePath
    , cabalComponents :: OneOrManyComponents CabalComponent
    } deriving (Show, Eq)

data CabalComponent = CabalComponent
    { cabalPath      :: FilePath
    , cabalComponent :: String
    , cabalComponentProject :: Maybe FilePath
    } deriving (Show, Eq)

data StackConfig = StackConfig
    { stackYaml       :: Maybe FilePath
    , stackComponents :: OneOrManyComponents StackComponent
    } deriving (Show, Eq)

data StackComponent = StackComponent
    { stackPath          :: FilePath
    , stackComponent     :: String
    , stackComponentYAML :: Maybe FilePath
    } deriving (Show, Eq)

data OneOrManyComponents component
  = SingleComponent String
  | ManyComponents [component]
  | NoComponent
  deriving (Show, Eq)

data DirectConfig = DirectConfig
    { arguments :: [String]
    } deriving (Generic, Show, Eq)

data BiosConfig = BiosConfig
    { callable     :: Callable
    , depsCallable :: Maybe Callable
    , ghcPath      :: Maybe FilePath
    } deriving (Show, Eq)

data Callable
    = Program FilePath
    | Shell String
    deriving (Show, Eq)

-----------------------------------------------------------------------
-- END OF DERIVED CODE
-----------------------------------------------------------------------

instance FromValue CradleConfig where
    fromValue = genericFromTable

instance FromValue CradleComponent where
    fromValue = parseTableFromValue $
        reqAlts [
            KeyCase Multi  "multi",
            KeyCase Cabal  "cabal",
            KeyCase Stack  "stack",
            KeyCase Direct "direct",
            KeyCase Bios   "bios",
            KeyCase None   "none"]

instance FromValue MultiSubComponent where
    fromValue = genericFromTable

instance FromValue CabalConfig where
    fromValue v@Toml.List'{} = CabalConfig Nothing . ManyComponents <$> fromValue v
    fromValue (Toml.Table' l t) = getComponentTable CabalConfig "cabalProject" l t
    fromValue _               = fail "cabal configuration expects table or array"

getComponentTable :: FromValue b => (Maybe FilePath -> OneOrManyComponents b -> a) -> Text -> l -> Toml.Table' l -> Matcher l a
getComponentTable con pathKey = parseTable $ con
    <$> optKey pathKey
    <*> pickKey [
        Key "component"  (fmap  SingleComponent . fromValue),
        Key "components" (fmap  ManyComponents  . fromValue),
        Else (pure NoComponent)]

instance FromValue CabalComponent where
    fromValue = parseTableFromValue $ CabalComponent
        <$> reqKey "path"
        <*> reqKey "component"
        <*> optKey "cabalProject"

instance FromValue StackConfig where
    fromValue v@Toml.List'{} = StackConfig Nothing . ManyComponents <$> fromValue v
    fromValue (Toml.Table' l t) = getComponentTable StackConfig "stackYaml" l t
    fromValue _ = fail "stack configuration expects table or array"

instance FromValue StackComponent where
    fromValue = parseTableFromValue $ StackComponent
        <$> reqKey "path"
        <*> reqKey "component"
        <*> optKey "stackYaml"

instance FromValue DirectConfig where
    fromValue = genericFromTable

instance FromValue BiosConfig where
    fromValue = parseTableFromValue $ BiosConfig
        <$> getCallable
        <*> getDepsCallable
        <*> optKey "with-ghc"
        where
            getCallable =
                reqAlts [
                    KeyCase Program "program",
                    KeyCase Shell   "shell"]
            getDepsCallable =
                optAlts [
                    KeyCase Program "dependency-program",
                    KeyCase Shell   "dependency-shell"]

data KeyCase a where
    KeyCase :: FromValue b => (b -> a) -> Text -> KeyCase a

reqAlts :: [KeyCase a] -> ParseTable l a
reqAlts xs = pickKey
    [Key key (fmap con . fromValue) | KeyCase con key <- xs]

optAlts :: [KeyCase a] -> ParseTable l (Maybe a)
optAlts xs = pickKey $
    [Key key (fmap (Just . con) . fromValue) | KeyCase con key <- xs] ++
    [Else (pure Nothing)]

instance FromValue NoneConfig where
    fromValue = parseTableFromValue (pure NoneConfig)

spec :: Spec
spec =
 do it "parses this project's hie.toml" $
        decode [quoteStr|
            dependencies = [
                "src/Toml/Lexer.x",
                "src/Toml/Parser.y",
            ]

            [[cradle.cabal]]
            path = "./src"
            component = "toml-parser:lib:toml-parser"

            [[cradle.cabal]]
            path = "./test"
            component = "toml-parser:test:unittests"

            [[cradle.cabal]]
            path = "./test-drivers/encoder"
            component = "toml-test-drivers:exe:TomlEncoder"

            [[cradle.cabal]]
            path = "./test-drivers/decoder"
            component = "toml-test-drivers:exe:TomlDecoder"

            [[cradle.cabal]]
            path = "./test-drivers/highlighter"
            component = "toml-test-drivers:exe:TomlHighlighter"
            |]
        `shouldBe`
        Success [] CradleConfig
            { cradle =
                Cabal
                    CabalConfig
                    { cabalProject = Nothing
                    , cabalComponents =
                        ManyComponents
                            [ CabalComponent
                                { cabalPath = "./src"
                                , cabalComponent = "toml-parser:lib:toml-parser"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test"
                                , cabalComponent = "toml-parser:test:unittests"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/encoder"
                                , cabalComponent = "toml-test-drivers:exe:TomlEncoder"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/decoder"
                                , cabalComponent = "toml-test-drivers:exe:TomlDecoder"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/highlighter"
                                , cabalComponent = "toml-test-drivers:exe:TomlHighlighter"
                                , cabalComponentProject = Nothing
                                }
                            ]
                    }
            , dependencies = Just ["src/Toml/Lexer.x" , "src/Toml/Parser.y"]
            }

    it "has focused error messages" $
        decode [quoteStr|
            [cradle.cabal]
            path = "./src"
            component = 42
            |]
        `shouldBe`
        (Failure ["3:13: type error. wanted: string got: integer in cradle.cabal.component"]
            :: Result String CradleConfig)

    it "detects unusd keys" $
        decode [quoteStr|
            [[cradle.multi]]
            path = "./src"
            [cradle.multi.config.cradle.cabal]
            component = "toml-parser:lib:toml-parser"
            thing1 = 10 # unused key for test case

            [[cradle.multi]]
            path = "./test"
            [cradle.multi.config.cradle.stack]
            component = "toml-parser:test:unittests"
            thing2 = 20 # more unused keys for test case
            thing3 = false
            |]
        `shouldBe`
        Success
            [ "5:1: unexpected key: thing1 in cradle.multi[0].config.cradle.cabal"
            , "11:1: unexpected key: thing2 in cradle.multi[1].config.cradle.stack"
            , "12:1: unexpected key: thing3 in cradle.multi[1].config.cradle.stack"

            ]
            CradleConfig
                { cradle =
                    Multi
                    [ MultiSubComponent
                        { path = "./src"
                        , config =
                            CradleConfig
                                { cradle =
                                    Cabal
                                    CabalConfig
                                        { cabalProject = Nothing
                                        , cabalComponents = SingleComponent "toml-parser:lib:toml-parser"
                                        }
                                , dependencies = Nothing
                                }
                        }
                    , MultiSubComponent
                        { path = "./test"
                        , config =
                            CradleConfig
                                { cradle =
                                    Stack
                                    StackConfig
                                        { stackYaml = Nothing
                                        , stackComponents = SingleComponent "toml-parser:test:unittests"
                                        }
                                , dependencies = Nothing
                                }
                        }
                    ]
                , dependencies = Nothing
                }

    it "parses things using components" $
        decode [quoteStr|
            dependencies = [
                "src/Toml/Lexer.x",
                "src/Toml/Parser.y",
            ]

            [cradle.cabal]
            cabalProject = "cabal.project"

            [[cradle.cabal.components]]
            path = "./src"
            component = "toml-parser:lib:toml-parser"

            [[cradle.cabal.components]]
            path = "./test"
            component = "toml-parser:test:unittests"

            [[cradle.cabal.components]]
            path = "./test-drivers/encoder"
            component = "toml-test-drivers:exe:TomlEncoder"

            [[cradle.cabal.components]]
            path = "./test-drivers/decoder"
            component = "toml-test-drivers:exe:TomlDecoder"

            [[cradle.cabal.components]]
            path = "./test-drivers/highlighter"
            component = "toml-test-drivers:exe:TomlHighlighter"
            |]
        `shouldBe`
        Success
            []
            CradleConfig
                { cradle =
                    Cabal
                    CabalConfig
                        { cabalProject = Just "cabal.project"
                        , cabalComponents =
                            ManyComponents
                            [ CabalComponent
                                { cabalPath = "./src"
                                , cabalComponent = "toml-parser:lib:toml-parser"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test"
                                , cabalComponent = "toml-parser:test:unittests"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/encoder"
                                , cabalComponent = "toml-test-drivers:exe:TomlEncoder"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/decoder"
                                , cabalComponent = "toml-test-drivers:exe:TomlDecoder"
                                , cabalComponentProject = Nothing
                                }
                            , CabalComponent
                                { cabalPath = "./test-drivers/highlighter"
                                , cabalComponent = "toml-test-drivers:exe:TomlHighlighter"
                                , cabalComponentProject = Nothing
                                }
                            ]
                        }
                , dependencies = Just [ "src/Toml/Lexer.x" , "src/Toml/Parser.y" ]
                }

    it "handles the none case" $
        decode [quoteStr|
            [cradle.none]|]
        `shouldBe`
        Success [] (CradleConfig {
            cradle = None NoneConfig,
            dependencies = Nothing})
