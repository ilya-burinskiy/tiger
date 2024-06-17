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
