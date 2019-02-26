{-# LANGUAGE ScopedTypeVariables #-}

module Util.UParse where

import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") $ symbol ")"

integer :: Parser Integer
integer = lexeme L.decimal

-- | parses a semicolon
semi :: Parser String
semi = symbol ";"

identifier :: Parser String
identifier = lexeme $ some alphaNumChar

filepath :: Parser String
filepath = lexeme $ some (alphaNumChar <|> char '.' <|> char '/')

identifier_alphaLed :: Parser String
identifier_alphaLed = lexeme $ (:) <$> letterChar <*> many alphaNumChar
  -- `(:) :: Char -> String -> String, and
  -- `letterChar :: Parser Char`, so
  -- `(:) <$> letterChar :: Parser (String -> String)

phrase :: Parser String -- | does not accept the empty string
phrase = concat . intersperse " " <$> some identifier

thisMany :: Int -> Char -> Parser ()
thisMany n c = string (replicate n c) <* notFollowedBy (char c)
               >> return ()

