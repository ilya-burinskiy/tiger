{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
  ( Dec (..),
    Expr (..),
    Id,
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
import Text.Megaparsec (Parsec, anySingleBut, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text.Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "//") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text.Text -> Parser Text.Text
symbol = Lexer.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseExpr :: Parser Expr
parseExpr =
  choice
    [ parseIfExpr,
      parseWhileExpr,
      parseForExpr,
      parseBreakExpr,
      parseLetExpr,
      try $ ExprSeq <$> parens parseExprSeq,
      parseOpExpr
    ]

-- TODO: define handle escape sequences
parseStringExpr :: Parser Expr
parseStringExpr = StringExpr <$> (char '"' *> many (anySingleBut '"') <* char '"')

parseIntExpr :: Parser Expr
parseIntExpr = IntExpr <$> lexeme Lexer.decimal

parseNilExpr :: Parser Expr
parseNilExpr = NilExpr <$ symbol "nil"

parseOpExpr :: Parser Expr
parseOpExpr =
  makeExprParser
    ( choice
        [ parens parseOpExpr,
          try parseCallExpr,
          parseLvalueExpr,
          parseIntExpr
        ]
    )
    operatorsTable

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

parseAssignExpr :: Parser Expr
parseAssignExpr = do
  lval <- parseLvalue
  void $ symbol ":="
  AssignExpr lval <$> parseExpr

parseCallExpr :: Parser Expr
parseCallExpr = do
  funcId <- parseId
  void $ symbol "("
  args <- parseExprList
  void $ symbol ")"
  return $ CallExpr funcId args

parseRecordInstanceExpr :: Parser Expr
parseRecordInstanceExpr = do
  typeId <- parseId
  void $ symbol "{"
  fieldList <- parseFieldList
  void $ symbol "}"
  return $ RecordInstanceExpr typeId fieldList

parseFieldList :: Parser [(Id, Expr)]
parseFieldList = do
  maybeFields <- optional $ do
    recordField <- parseRecordField
    maybeRestOfRecordFields <- optional $ many (void (symbol ",") >> parseRecordField)
    case maybeRestOfRecordFields of
      Just recordFields -> return $ recordField : recordFields
      Nothing -> return [recordField]
  case maybeFields of
    Just fields -> return fields
    Nothing -> return []

parseRecordField :: Parser (Id, Expr)
parseRecordField = do
  id' <- parseId
  void $ symbol "="
  (,) id' <$> parseExpr

parseArrayInstanceExpr :: Parser Expr
parseArrayInstanceExpr = do
  typeId <- parseId
  void $ symbol "["
  idxExpr <- parseExpr
  void $ symbol "]"
  void $ symbol "of"
  ArrayInstanceExpr typeId idxExpr <$> parseExpr

parseDecList :: Parser [Dec]
parseDecList = do
  typeDec <- parseTypeDeclaration <|> parseVariableDeclaration
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
    typeId <- parseId
    void $ lexeme $ char '='
    AliasTypeDec typeId <$> parseId
      <|> ( RecordTypeDec typeId
              <$> (lexeme (char '{') *> parseTypeFields <* lexeme (char '}'))
          )
      <|> ( ArrayTypeDec typeId
              <$> (lexeme (string "array") *> lexeme (string "of") *> parseId)
          )

-- vardec := `var` id `:=` expr | `var` id`:`typeid `:=` expr
parseVariableDeclaration :: Parser Dec
parseVariableDeclaration =
  try
    ( do
        void $ lexeme $ string "var"
        varId <- parseId
        void $ lexeme $ string ":="
        VarDec varId <$> parseExpr
    )
    <|> ( do
            void $ lexeme $ string "var"
            varId <- parseId
            void $ lexeme $ char ':'
            typeId <- parseId
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
        funId <- parseId
        void $ lexeme $ char '('
        typeFields <- parseTypeFields
        void $ lexeme $ char ')'
        void $ lexeme $ char '='
        FunDec funId typeFields <$> parseExpr
    )
    <|> ( do
            void $ lexeme $ string "function"
            funId <- parseId
            void $ lexeme $ char '('
            typeFields <- parseTypeFields
            void $ lexeme $ char ')'
            void $ lexeme $ char ':'
            returnType <- parseId
            void $ lexeme $ char '='
            TypedFunDec funId typeFields returnType <$> parseExpr
        )

parseTypeField :: Parser TypeField
parseTypeField = do
  id' <- parseId
  void $ lexeme $ char ':'
  TypeField id' <$> parseId

parseId :: Parser [Char]
parseId = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

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

parseWhileExpr :: Parser Expr
parseWhileExpr = do
  void $ symbol "while"
  cond <- parseExpr
  void $ symbol "do"
  WhileExpr cond <$> parseExpr

parseForExpr :: Parser Expr
parseForExpr = do
  void $ symbol "for"
  id' <- parseId
  void $ symbol ":="
  initVal <- parseExpr
  void $ symbol "to"
  finalVal <- parseExpr
  void $ symbol "do"
  ForExpr id' initVal finalVal <$> parseExpr

-- lvalue := id lvalue'
parseLvalueExpr :: Parser Expr
parseLvalueExpr = LvalExpr <$> parseLvalue

parseLvalue :: Parser Lvalue
parseLvalue = do
  id' <- parseId
  restOfLvalueExpr <- parseRestOfLvalueExpr
  case restOfLvalueExpr of
    Just rest ->
      return $ foldl' (\res lvalConstruct -> lvalConstruct res) (IdLvalue id') rest
    Nothing -> return $ IdLvalue id'

-- lvalue' := `.` id lvalue' | `[` expr `]` lvalue' | eps
parseRestOfLvalueExpr :: Parser (Maybe [Lvalue -> Lvalue])
parseRestOfLvalueExpr =
  optional
    ( many $
        (flip RecordLvalue <$> (lexeme (char '.') *> parseId))
          <|> (flip ArrayLvalue <$> (lexeme (char '[') *> parseExpr <* lexeme (char ']')))
    )

parseExprSeq :: Parser [Expr]
parseExprSeq = do
  maybeExprSeq <- optional $ do
    expr <- parseExpr
    maybeRestOfExprSeq <- optional $ many (void (lexeme $ char ';') >> parseExpr)
    case maybeRestOfExprSeq of
      Just restOfExprSeq -> return $ expr : restOfExprSeq
      Nothing -> return [expr]
  case maybeExprSeq of
    Just exprSeq -> return exprSeq
    Nothing -> return []

parseExprList :: Parser [Expr]
parseExprList = do
  maybeExprList <- optional $ do
    expr <- parseExpr
    maybeRestOfExprSeq <- optional $ many (void (lexeme $ char ',') >> parseExpr)
    case maybeRestOfExprSeq of
      Just restOfExprSeq -> return $ expr : restOfExprSeq
      Nothing -> return [expr]
  case maybeExprList of
    Just exprList -> return exprList
    Nothing -> return []

parseBreakExpr :: Parser Expr
parseBreakExpr = BreakExpr <$ symbol "break"

parseLetExpr :: Parser Expr
parseLetExpr = do
  void $ lexeme $ string "let"
  decs <- parseDecList
  void $ lexeme $ string "in"
  exprSeq <- parseExprSeq
  void $ lexeme $ string "end"
  return $ LetExpr decs exprSeq
