module Parser where

import Lexer
import Syntax

import Text.Parsec (parse)
import Text.Parsec.String (Parser, parseFromFile)
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
  base <- Comb.option "Object" (reserved "inherits" >> typeIdentifier)
  features <- braces $ many feature
  semi
  return $ Class name base features

feature :: Parser Feature
feature = try method <|> attribute

method :: Parser Feature
method = do
  name <- objectIdentifier
  formals <- parens $ commaSep formal
  colon
  result <- typeIdentifier
  body <- braces expr
  semi
  return $ Method name formals result body

attribute :: Parser Feature
attribute = do
  name <- objectIdentifier
  colon
  type_ <- typeIdentifier
  init <- Comb.option NoExpr (reservedOp "<-" >> expr)
  semi
  return $ Attribute name type_ init

formal :: Parser Formal
formal = do
  name <- objectIdentifier
  colon
  type_ <- typeIdentifier
  return $ Formal name type_


exprs :: Parser Expr
exprs = do
  expressions <- braces $ Comb.many1 stmtExpr
  return $ Compound expressions

stmtExpr :: Parser Expr
stmtExpr = do
  e <- expr
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
  e <- expr
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
expr = Expr.buildExpressionParser exprTable baseExpr

baseExpr :: Parser Expr
baseExpr = letExpr
  <|> newExpr
  <|> try assignment
  <|> term

letExpr :: Parser Expr
letExpr = do
  reserved "let"
  name <- objectIdentifier
  colon
  t <- typeIdentifier
  init <- Comb.option NoExpr (reservedOp "<-" >> expr)
  tail <- letTail
  return $ Let name t init tail

letTail :: Parser Expr
letTail = letBinding <|> letBody

letBinding :: Parser Expr
letBinding = do
  comma
  name <- objectIdentifier
  colon
  t <- typeIdentifier
  init <- Comb.option NoExpr (reservedOp "<-" >> expr)
  tail <- letTail
  return $ Let name t init tail

letBody :: Parser Expr
letBody = do
  reserved "in"
  expr

term :: Parser Expr
term = idTerm <|> exprTerm

exprTerm :: Parser Expr
exprTerm = do
  e <- (int <|> bool <|> str <|> parens expr <|> exprs)
  callTail e

idTerm :: Parser Expr
idTerm = do
  id <- objectIdentifier
  parenthesis <- lookAhead $ Comb.optionMaybe $ char '('
  case parenthesis of
    Just _ -> do
      actuals <- parens $ commaSep expr
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
staticCall e = do
  staticType <- typeIdentifier
  reservedOp "."
  callName <- objectIdentifier
  actuals <- parens $ commaSep expr
  callTail $ StaticCall e staticType callName actuals

call :: Expr -> Parser Expr
call e = do
  callName <- objectIdentifier
  actuals <- parens $ commaSep expr
  callTail $ Call e callName actuals

int :: Parser Expr
int = do
  value <- integer
  return $ Int value

bool :: Parser Expr
-- first character of bool constanct have to be in lower case
bool = try (char 't' >> reserved "rue" >> (return $ BoolConst True))
  <|> try (char 'f' >> reserved "alse" >> (return $ BoolConst False))

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


parseWith :: Parser a -> String -> Either ParseError a
parseWith kind = parse (contents kind) "<stdin>"

parseExpr :: String -> Either ParseError Expr
parseExpr = parseWith expr

parseClass :: String -> Either ParseError Class
parseClass = parseWith class_

parseProgram :: String -> Either ParseError Program
parseProgram = parseWith program


data TopLevel = P Program | E Expr deriving (Show)

toplevel :: Parser TopLevel
toplevel = do
  programResult <- Comb.optionMaybe program
  case programResult of
    Just p -> return $ P p
    Nothing -> liftM E $ expr

parseTopLevel :: String -> Either ParseError TopLevel
parseTopLevel = parse (contents toplevel) "<stdio>"

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile = parseFromFile (contents program)
