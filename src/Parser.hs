module Parser where

import Lexer
import Syntax

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (eof)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tok

binary name fun assoc = Expr.Infix (reservedOp name >> return (BinOp fun)) assoc

exprTable = [[binary "*" Mul Expr.AssocLeft,
              binary "/" Div Expr.AssocLeft],
             [binary "+" Plus Expr.AssocLeft,
              binary "-" Minus Expr.AssocLeft]]

expr :: Parser Expr
expr = Expr.buildExpressionParser exprTable term

term :: Parser Expr
term =
  int
  <|> Parser.id
  <|> parens expr

int :: Parser Expr
int = do
  value <- integer
  return $ Int value

id :: Parser Expr
id = do
  name <- identifier
  return $ Id name


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r


parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s
