module Parser where

import Lexer
import Syntax

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>), try, many, lookAhead)
import Text.Parsec.Char (char)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Combinator as Comb
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Tok

import Control.Monad (liftM)

program :: Parser Program
program = liftM Program $ Comb.many1 class_

class_ :: Parser Class
class_ = do
  reserved "class"
  name <- typeIdentifier
  base <- Comb.option "NO_CLASS" (reserved "inherits" >> typeIdentifier)
  features <- braces $ many feature
  semi
  return $ Class name base features

-- just Method is implemented right now
feature :: Parser Feature
feature = do
  name <- objectIdentifier
  formals <- parens $ commaSep formal
  colon
  result <- typeIdentifier
  body <- braces topLevelExpr
  semi
  return $ Method name formals result body

formal :: Parser Formal
formal = do
  name <- objectIdentifier
  colon
  type_ <- typeIdentifier
  return $ Formal name type_


topLevelExpr :: Parser Expr
topLevelExpr = exprs
  <|> newExpr
  <|> try assignment
  <|> expr

exprs :: Parser Expr
exprs = do
  expressions <- braces $ many stmtExpr
  return $ Compound expressions

stmtExpr :: Parser Expr
stmtExpr = do
  e <- topLevelExpr
  semi
  return e

newExpr :: Parser Expr
newExpr = do
  reservedOp "new"
  type_ <- typeIdentifier
  return $ New type_

assignment :: Parser Expr
assignment = do
  name <- objectIdentifier
  reservedOp "<-"
  e <- topLevelExpr
  return $ Assignment name e

binary name fun assoc = Expr.Infix (reservedOp name >> return (BinExpr fun)) assoc
prefix name fun = Expr.Prefix (reservedOp name >> return (UnExpr fun))

exprTable = [[prefix "~" Complement],
             [prefix "isvoid" IsVoid],
             [binary "*" Mul Expr.AssocLeft,
              binary "/" Div Expr.AssocLeft],
             [binary "+" Plus Expr.AssocLeft,
              binary "-" Minus Expr.AssocLeft],
             [binary "<=" Le Expr.AssocNone,
              binary "<" Lt Expr.AssocNone,
              binary "=" Eq Expr.AssocNone],
             [prefix "not" Not]]

expr :: Parser Expr
expr = Expr.buildExpressionParser exprTable term

term :: Parser Expr
term = parens topLevelExpr <|> simpleTerm

simpleTerm :: Parser Expr
simpleTerm = constTerm <|> idTerm

constTerm :: Parser Expr
constTerm = do
  e <- (int <|> bool <|> str)
  callTail e

idTerm :: Parser Expr
idTerm = do
  id <- objectIdentifier
  parenthesis <- lookAhead $ Comb.optionMaybe $ char '('
  case parenthesis of
    Just _ -> do
      actuals <- parens $ commaSep topLevelExpr
      callTail $ Call (Id "self") id actuals
    Nothing -> callTail $ Id id

-- Tries to parse call or static call. If fails, just return expr
callTail :: Expr -> Parser Expr
callTail expr = do
  at <- Comb.optionMaybe $ reservedOp "@"
  case at of
    Just _ -> staticCall expr
    Nothing -> do
      dot <- Comb.optionMaybe $ reservedOp "."
      case dot of
        Just _ -> call expr
        Nothing -> return expr

staticCall :: Expr -> Parser Expr
staticCall expr = do
  staticType <- typeIdentifier
  reservedOp "."
  callName <- objectIdentifier
  actuals <- parens $ commaSep topLevelExpr
  callTail $ StaticCall expr staticType callName actuals

call :: Expr -> Parser Expr
call expr = do
  callName <- objectIdentifier
  actuals <- parens $ commaSep topLevelExpr
  callTail $ Call expr callName actuals

int :: Parser Expr
int = do
  value <- integer
  return $ Int value

bool :: Parser Expr
bool = (reserved "true" >> (return $ BoolConst True))
  <|> (reserved "false" >> (return $ BoolConst False))

str :: Parser Expr
str = do
  value <- string
  return $ StringConst value

id :: Parser Expr
id = do
  name <- objectIdentifier
  return $ Id name


contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  Comb.eof
  return r


parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents topLevelExpr) "<stdin>"

parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents program) "<stdin>"


data TopLevel = P Program | E Expr deriving (Show)

toplevel :: Parser TopLevel
toplevel = do
  programResult <- Comb.optionMaybe program
  case programResult of
    Just p -> return $ P p
    Nothing -> liftM E $ topLevelExpr

parseTopLevel :: String -> Either ParseError TopLevel
parseTopLevel = parse (contents toplevel) "<stdio>"
