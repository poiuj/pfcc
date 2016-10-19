module Parser where

import Lexer
import Syntax

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (eof, option, optionMaybe)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tok

import Control.Monad (liftM)

program :: Parser Program
program = liftM Program $ semiSep1 class_

class_ :: Parser Class
class_ = do
  reserved "class"
  name <- typeIdentifier
  base <- option "NO_CLASS" (reserved "inherits" >> typeIdentifier)
  features <- braces $ semiSep feature
  return $ Class name base features

-- just Method is implemented right now
feature :: Parser Feature
feature = do
  name <- idIdentifier
  formals <- parens $ commaSep formal
  reservedOp ":"
  result <- typeIdentifier
  body <- braces expr
  return $ Method name formals result body

formal :: Parser Formal
formal = do
  name <- idIdentifier
  reservedOp ":"
  type_ <- typeIdentifier
  return $ Formal name type_


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
  name <- idIdentifier
  return $ Id name


contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r


parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents program) "<stdin>"


data TopLevel = P Program | E Expr deriving (Show)

toplevel :: Parser TopLevel
toplevel = do
  programResult <- optionMaybe program
  case programResult of
    Just p -> return $ P p
    Nothing -> liftM E $ expr

parseTopLevel :: String -> Either ParseError TopLevel
parseTopLevel = parse (contents toplevel) "<stdio>"
