module Lexer where

import Data.Char (isLower, isUpper)

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many, (<|>), lookAhead)
import Text.Parsec.Char (oneOf, noneOf, char)
import Text.Parsec.Combinator (between)
import Text.Parsec.Prim

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as Char

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {
  Tok.commentStart = "(*"
  , Tok.commentEnd = "*)"
  , Tok.commentLine = "--"
  , Tok.opStart = char '<'
  , Tok.opLetter = oneOf "=-"
  , Tok.reservedOpNames = ["."
                          , "@"
                          , "~"
                          , "isvoid"
                          , "*", "/"
                          , "+", "-"
                          , "<=", "<", "="
                          , "not"
                          , "<-"]
  , Tok.reservedNames = ["true"
                        , "false"
                        , "class"
                        , "inherits"
                        , "let"
                        , "in"
                        , "new"]
  }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

integer :: Parser Integer
integer = (Tok.lexeme lexer) $ Tok.decimal lexer

identifier :: (Char -> Bool) -> String -> Parser String
identifier firstLetterPred errorMsg = do
  id <- lookAhead $ Tok.identifier lexer
  if firstLetterPred $ head id
    then (Tok.identifier lexer >> return id)
    else fail errorMsg

objectIdentifier :: Parser String
objectIdentifier = identifier isLower "ID expected"

typeIdentifier :: Parser String
typeIdentifier = identifier isUpper "TYPE expected"

string :: Parser String
string = do
  between (char '"') (char '"') $ many validStringElement

validStringElement :: Parser Char
validStringElement = escapedChar <|> validChar

escaped :: Char -> Char -> Parser Char
escaped c ec = char c >> return ec

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  (escaped 'b' '\b') <|> (escaped 't' '\t') <|> (escaped 'n' '\n') <|> (escaped 'f' '\f') <|> validEscapedChar

validEscapedChar :: Parser Char
validEscapedChar = char '"' <|> char '\n' <|> validChar

validChar :: Parser Char
validChar = noneOf "\"\n\0"

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semi :: Parser String
semi = Tok.semi lexer

colon :: Parser String
colon = Tok.colon lexer

comma :: Parser String
comma = Tok.comma lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
