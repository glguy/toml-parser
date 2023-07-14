module LexerSpec (spec) where

import Data.Map qualified as Map
import Test.Hspec (it, shouldBe, Spec)
import Toml (parse, Value(Integer))

spec :: Spec
spec =
 do it "handles special cased control character" $
        parse "x = '\SOH'"
        `shouldBe`
        Left "1:6: lexical error: unexpected '\\SOH'"

    -- These seem boring, but they provide test coverage of an error case in the state machine
    it "handles unexpected '}'" $
        parse "}"
        `shouldBe`
        Left "1:1: parse error: unexpected '}'"

    it "handles unexpected '{'" $
        parse "{"
        `shouldBe`
        Left "1:1: parse error: unexpected '{'"

    it "accepts tabs" $
        parse "x\t=\t1"
        `shouldBe`
        Right (Map.singleton "x" (Integer 1))

    it "computes columns correctly with tabs" $
        parse "x\t=\t="
        `shouldBe`
        Left "1:17: parse error: unexpected '='"

    it "detects non-scalars in strings" $
        parse "x = \"\\udfff\""
        `shouldBe`
        Left "1:6: lexical error: non-scalar unicode escape"

    it "catches unclosed [" $
        parse "x = [1,2,3"
        `shouldBe`
        Left "1:11: parse error: unexpected end-of-input"

    it "catches unclosed {" $
        parse "x = { y"
        `shouldBe`
        Left "1:8: parse error: unexpected end-of-input"

    it "catches unclosed \"" $
        parse "x = \"abc"
        `shouldBe`
        Left "1:5: lexical error: unterminated string literal"

    it "catches unclosed \"\"\"" $
        parse "x = \"\"\"test"
        `shouldBe`
        Left "1:5: lexical error: unterminated multi-line string literal"

    it "handles escapes at the end of input" $
        parse "x = \"\\"
        `shouldBe`
        Left "1:7: lexical error: unexpected end-of-input"

    it "handles invalid escapes" $
        parse "x = \"\\p\""
        `shouldBe`
        Left "1:7: lexical error: unexpected 'p'"
