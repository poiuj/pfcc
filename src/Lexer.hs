module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char (oneOf)

import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
    Tok.commentStart = "(*"
  , Tok.commentEnd = "*)"
  , Tok.commentLine = "--"
  , Tok.reservedOpNames = ["+", "-", "*", "/"]
  }

integer :: Parser Integer
integer = (Tok.lexeme lexer) $ Tok.decimal lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer


tryLex :: Parser a -> String -> Either ParseError a
tryLex p input = runParser p () "<stdin>" input

test :: String -> String
test input =
  case tryLex integer input of
    Right val -> show val
    Left _ ->
      case tryLex identifier input of
        Right val -> show val
        Left _ -> "error"
