module Tiger.ParserSpec where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Text.Megaparsec (parse)
import Tiger.Ast (Expr (..))
import Tiger.Parser
  ( parseStringExpr,
    parseIntExpr,
    parseNilExpr
  )

spec :: Spec
spec = do
  describe "parser" $ do
    describe "parseStringExpr" $ do
      it "parses string" $ do
        parse parseStringExpr "" "\"abcd\"" `shouldParse` StringExpr "abcd"
    describe "parseIntExpr" $ do
      it "parses int" $ do
        parse parseIntExpr "" "123" `shouldParse` IntExpr 123
    describe "parseNilExpr" $ do
      it "parses nil expr" $ do
        parse parseNilExpr "" "nil" `shouldParse` NilExpr
      it "does not parse nil followed by num" $
        parse parseNilExpr ""  `shouldFailOn` "nil1"
      it "does not parse nil followed by char" $
        parse parseNilExpr ""  `shouldFailOn` "nila"
      it "does not parse nil followed by \"_\"" $
        parse parseNilExpr ""  `shouldFailOn` "nil_"
