{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
  ( Dec (..),
    Expr (..),
    Lvalue (..),
    TypeField (..),
  )
import Control.Applicative (optional, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators (between, choice, many)
import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Data.Foldable (Foldable (foldl'))
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, try)
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
  parseIfExpr
    <|> parseOpExpr

parseOpExpr :: Parser Expr
parseOpExpr =
  makeExprParser (choice [parens parseOpExpr, parseLvalueExpr, parseInt]) operatorsTable

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

parseDecList :: Parser [Dec]
parseDecList = do
  typeDec <- parseTypeDeclaration
  maybeRestOfTypeDecs <- optional (many parseTypeDeclaration)
  case maybeRestOfTypeDecs of
    Just restOfTypeDecs -> return $ typeDec : restOfTypeDecs
    Nothing -> return [typeDec]

-- type_declaration := `type` typeid `=` type
-- type := typeid | `{` typefields `}` | `array` `of` typeid
parseTypeDeclaration :: Parser Dec
parseTypeDeclaration =
  do
    void $ lexeme $ string "type"
    typeId <- parseID
    void $ lexeme $ char '='
    AliasTypeDec typeId <$> parseID
      <|> ( RecordTypeDec typeId
              <$> (lexeme (char '{') *> parseTypeFields <* lexeme (char '}'))
          )
      <|> ( ArrayTypeDec typeId
              <$> (lexeme (string "array") *> lexeme (string "of") *> parseID)
          )

-- vardec := `var` id `:=` expr | `var` id`:`typeid `:=` expr
parseVariableDeclaration :: Parser Dec
parseVariableDeclaration =
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
parseFunctionDeclaration :: Parser Dec
parseFunctionDeclaration =
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
    <|> ( do
            void $ lexeme $ string "function"
            funId <- parseID
            void $ lexeme $ char '('
            typeFields <- parseTypeFields
            void $ lexeme $ char ')'
            void $ lexeme $ char ':'
            returnType <- parseID
            void $ lexeme $ char '='
            TypedFunDec funId typeFields returnType <$> parseExpr
        )

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

-- ifexpr := `if` expr `then` expr `else` expr
--           `if` expr `then` expr
parseIfExpr :: Parser Expr
parseIfExpr =
  try
    ( do
        void $ lexeme $ string "if"
        cond <- parseExpr
        void $ lexeme $ string "then"
        onTrue <- parseExpr
        void $ lexeme $ string "else"
        IfThenElseExpr cond onTrue <$> parseExpr
    )
    <|> ( do
            void $ lexeme $ string "if"
            cond <- parseExpr
            void $ lexeme $ string "then"
            IfThenExpr cond <$> parseExpr
        )

-- lvalue := id lvalue'
parseLvalueExpr :: Parser Expr
parseLvalueExpr = do
  id' <- parseID
  restOfLvalueExpr <- parseRestOfLvalueExpr
  case restOfLvalueExpr of
    Just rest ->
      return $
        LvalExpr $
          foldl' (\res lvalConstruct -> lvalConstruct res) (IdLvalue id') rest
    Nothing -> return $ LvalExpr (IdLvalue id')

-- lvalue' := `.` id lvalue' | `[` expr `]` lvalue' | eps
parseRestOfLvalueExpr :: Parser (Maybe [Lvalue -> Lvalue])
parseRestOfLvalueExpr =
  optional
    ( many $
        (flip RecordLvalue <$> (lexeme (char '.') *> parseID))
          <|> (flip ArrayLvalue <$> (lexeme (char '[') *> parseExpr <* lexeme (char ']')))
    )
