module Lexer where

import Data.Char (isLower, isUpper)

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many, (<|>))
import Text.Parsec.Char (oneOf, char)
import Text.Parsec.Error (ParseError)
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
                        , "inherits"]
  }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

integer :: Parser Integer
integer = (Tok.lexeme lexer) $ Tok.decimal lexer

identifier :: (Char -> Bool) -> String -> Parser String
identifier firstLetterPred errorMsg = do
  id <- Tok.identifier lexer
  if firstLetterPred $ head id
    then return id
    else fail errorMsg

objectIdentifier :: Parser String
objectIdentifier = identifier isLower "ID expected"

typeIdentifier :: Parser String
typeIdentifier = identifier isUpper "TYPE expected"

string :: Parser String
string = Tok.stringLiteral lexer

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

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
