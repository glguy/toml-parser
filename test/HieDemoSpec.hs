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

import Control.Applicative (optional)
import GHC.Generics ( Generic )
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Value(Table, Array), Table, Result(..), decode)
import Toml.FromValue
import Toml.FromValue.Generic ( genericFromTable )

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
  fromValue = defaultTableFromValue

instance FromTable CradleConfig where
  fromTable = genericFromTable

instance FromValue CradleComponent where
  fromValue = defaultTableFromValue

instance FromTable CradleComponent where
    fromTable = runParseTable $
      pickKey [
        Key "multi"  (fmap Multi  . fromValue),
        Key "cabal"  (fmap Cabal  . fromValue),
        Key "stack"  (fmap Stack  . fromValue),
        Key "direct" (fmap Direct . fromValue),
        Key "bios"   (fmap Bios   . fromValue),
        Key "none"   (fmap None   . fromValue)]

instance FromValue MultiSubComponent where
  fromValue = defaultTableFromValue

instance FromTable MultiSubComponent where
  fromTable = genericFromTable

instance FromValue CabalConfig where
  fromValue v@Toml.Array{} = CabalConfig Nothing . ManyComponents <$> fromValue v
  fromValue (Toml.Table t)  = getComponentTable CabalConfig "cabalProject" t
  fromValue _               = fail "cabal configuration expects table or array"

getComponentTable :: FromValue b => (Maybe FilePath -> OneOrManyComponents b -> a) -> String -> Toml.Table -> Matcher a
getComponentTable con pathKey = runParseTable $
  con <$> optKey pathKey
      <*> pickKey [
          Key "component"  (fmap  SingleComponent . fromValue),
          Key "components" (fmap  ManyComponents  . fromValue),
          Else (pure NoComponent)
          ]

instance FromValue CabalComponent where
  fromValue = defaultTableFromValue

instance FromTable CabalComponent where
  fromTable = runParseTable $ CabalComponent
    <$> reqKey "path"
    <*> reqKey "component"
    <*> optKey "cabalProject"

instance FromValue StackConfig where
  fromValue v@Toml.Array{} = StackConfig Nothing . ManyComponents <$> fromValue v
  fromValue (Toml.Table t) = getComponentTable StackConfig "stackYaml" t
  fromValue _              = fail "stack configuration expects table or array"

instance FromValue StackComponent where
  fromValue = defaultTableFromValue

instance FromTable StackComponent where
  fromTable = runParseTable $ StackComponent
    <$> reqKey "path"
    <*> reqKey "component"
    <*> optKey "stackYaml"

instance FromValue DirectConfig where
  fromValue = defaultTableFromValue

instance FromTable DirectConfig where
  fromTable = genericFromTable

instance FromValue BiosConfig where
  fromValue = defaultTableFromValue

instance FromTable BiosConfig where
  fromTable = runParseTable $ BiosConfig
    <$> getCallable
    <*> getDepsCallable
    <*> optKey "with-ghc"
    where
      getCallable =
        pickKey [
          Key "program" (fmap Program . fromValue),
          Key "shell"   (fmap Shell   . fromValue)]
      getDepsCallable =
        optional (pickKey [
          Key "dependency-program" (fmap Program . fromValue),
          Key "dependency-shell"   (fmap Shell   . fromValue)]
        )

instance FromValue NoneConfig where
  fromValue = defaultTableFromValue

instance FromTable NoneConfig where
  fromTable = genericFromTable

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
        (Failure ["type error. wanted: string got: integer in top.cradle.cabal.component"]
            :: Result CradleConfig)

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
            [ "unexpected key: thing1 in top.cradle.multi[0].config.cradle.cabal"
            , "unexpected keys: thing2, thing3 in top.cradle.multi[1].config.cradle.stack"
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
