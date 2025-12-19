{-# Language OverloadedStrings #-}
module LexerSpec (spec) where

import Data.Text (Text)
import Test.Hspec (it, shouldBe, Spec)
import Toml
import Toml.Schema (table, (.=))

parse_ :: Text -> Either String Table
parse_ str = forgetTableAnns <$> parse str

spec :: Spec
spec =
 do it "handles special cased control character" $
        parse "x = '\SOH'"
        `shouldBe`
        Left "1:6: lexical error: control characters prohibited"

    it "recommends escapes for control characters (1)" $
        parse "x = \"\SOH\""
        `shouldBe`
        Left "1:6: lexical error: control characters must be escaped, use: \\x01"

    it "recommends escapes for control characters (2)" $
        parse "x = \"\DEL\""
        `shouldBe`
        Left "1:6: lexical error: control characters must be escaped, use: \\x7F"

    it "reports incomplete escapes (2)" $
        parse "x = \"\\x1\""
        `shouldBe`
        Left "1:6: lexical error: \\x requires exactly 2 hex digits"

    it "reports incomplete escapes (4)" $
        parse "x = \"\\u1\""
        `shouldBe`
        Left "1:6: lexical error: \\u requires exactly 4 hex digits"

    it "reports incomplete escapes (8)" $
        parse "x = \"\\U1\""
        `shouldBe`
        Left "1:6: lexical error: \\U requires exactly 8 hex digits"

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
        parse_ "x\t=\t1"
        `shouldBe`
        Right (table ["x" .= Integer 1])

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
        Left "1:5: lexical error: unterminated basic string"

    it "catches unclosed \"\"\"" $
        parse "x = \"\"\"test"
        `shouldBe`
        Left "1:5: lexical error: unterminated multi-line basic string"

    it "catches unclosed '" $
        parse "x = 'abc\ny = 2"
        `shouldBe`
        Left "1:9: lexical error: unexpected end-of-line"

    it "catches unclosed '" $
        parse "x = 'abc"
        `shouldBe`
        Left "1:5: lexical error: unterminated literal string"

    it "catches unclosed '''" $
        parse "x = '''test\n\n"
        `shouldBe`
        Left "1:5: lexical error: unterminated multi-line literal string"

    it "handles escapes at the end of input" $
        parse "x = \"\\"
        `shouldBe`
        Left "1:6: lexical error: incomplete escape sequence"

    it "handles invalid escapes" $
        parse "x = \"\\p\""
        `shouldBe`
        Left "1:6: lexical error: unknown escape sequence"

    it "allows multi-byte characters in ''' strings" $
        parse_ "x = '''§'''"
        `shouldBe`
        Right (table ["x" .= Text "§"])
