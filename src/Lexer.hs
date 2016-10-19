module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many, (<|>))
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as Char

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
  Tok.commentStart = "(*"
  , Tok.commentEnd = "*)"
  , Tok.commentLine = "--"
  , Tok.reservedOpNames = ["+", "-", "*", "/", ":"]
  , Tok.reservedNames = ["class", "inherits"]
  }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

integer :: Parser Integer
integer = (Tok.lexeme lexer) $ Tok.decimal lexer

identifier :: Parser Char -> Parser String
identifier firstLetterParser = do
  first <- firstLetterParser
  tail <- many (Char.alphaNum <|> Char.char '_')
  whiteSpace
  return $ first : tail

idIdentifier :: Parser String
idIdentifier = identifier Char.lower

typeIdentifier :: Parser String
typeIdentifier = identifier Char.upper

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
