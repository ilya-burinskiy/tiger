{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
  ( Expr (DivExpr, IdExpr, IntExpr, NegExpr, ProductExpr, SubExpr, SumExpr),
    FunDec (FunDec),
    Type (..),
    TypeDec (..),
    TypeField (..),
    VarDec (..),
  )
import Control.Applicative (optional, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators (between, choice, many)
import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text.Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "//") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text.Text -> Parser Text.Text
symbol = Lexer.symbol spaceConsumer

parseExpr :: Parser Expr
parseExpr =
  makeExprParser (choice [parens parseExpr, parseIdExpr, parseInt]) operatorsTable

parseIdExpr :: Parser Expr
parseIdExpr = IdExpr <$> lexeme (parseID <?> "variable")

parseInt :: Parser Expr
parseInt = IntExpr <$> lexeme Lexer.decimal

parens :: Parser a -> Parser a
parens = between (symbol ")") (symbol ")")

operatorsTable :: [[Operator Parser Expr]]
operatorsTable =
  [ [prefix "-" NegExpr, prefix "+" id],
    [binary "*" ProductExpr, binary "/" DivExpr],
    [binary "+" SumExpr, binary "-" SubExpr]
  ]

binary :: Text.Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text.Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

parseTypeDec :: Parser TypeDec
parseTypeDec = do
  void $ lexeme $ string "type"
  typeId <- parseID
  void $ lexeme $ char '='
  TypeDec typeId <$> parseType

-- ty := typeid | `{` typefields `}` | `array` `of` typeid
parseType :: Parser Type
parseType =
  TypeId <$> parseID
    <|> char '{' *> (TypeFields <$> parseTypeFields) <* char '}'
    <|> ArrayType <$> (string "array" *> string "of" *> parseID)

parseTypeField :: Parser TypeField
parseTypeField = do
  id' <- parseID
  void $ lexeme $ char ':'
  TypeField id' <$> parseID

parseID :: Parser [Char]
parseID = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- typefields := [id`:` typeid [`,` id`:` typeid]*]
parseTypeFields :: Parser [TypeField]
parseTypeFields = do
  maybeTypeFields <- optional $ do
    typeField <- parseTypeField
    maybeRestTypeFields <- optional $ many (void (lexeme $ char ',') >> parseTypeField)
    case maybeRestTypeFields of
      Just typeFields -> return $ typeField : typeFields
      Nothing -> return [typeField]
  case maybeTypeFields of
    Just typeFields -> return typeFields
    Nothing -> return []

-- vardec := `var` id `:=` expr | `var` id`:`typeid `:=` expr
parseVarDec :: Parser VarDec
parseVarDec =
  try
    ( do
        void $ lexeme $ string "var"
        varId <- parseID
        void $ lexeme $ string ":="
        VarDec varId <$> parseExpr
    )
    <|> ( do
            void $ lexeme $ string "var"
            varId <- parseID
            void $ lexeme $ char ':'
            typeId <- parseID
            void $ lexeme $ string ":="
            TypedVarDec varId typeId <$> parseExpr
        )

-- fundec := `function` id `(` typefields `)` `=` exp |
--           `function` id `(` typefields `)` `:` typeid `=` exp
parseFunDec :: Parser FunDec
parseFunDec =
  try
    ( do
        void $ lexeme $ string "function"
        funId <- parseID
        void $ lexeme $ char '('
        typeFields <- parseTypeFields
        void $ lexeme $ char ')'
        void $ lexeme $ char '='
        FunDec funId typeFields <$> parseExpr
    )
