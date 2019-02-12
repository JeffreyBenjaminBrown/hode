{-# LANGUAGE ScopedTypeVariables #-}

module Util.Parse where

import           Control.Monad (void)
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

reservedWord :: String -> Parser ()
reservedWord w = lexeme . try
  $ string w *> notFollowedBy alphaNumChar
  -- `string` backtracks upon failure

reservedWords :: [String]
reservedWords = ["if","then","else","while","do"
                ,"skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= notReserved) where
    -- without `try`, expressions like many identifier would
    -- fail on such identifiers instead of just stopping.
  (p :: Parser String) =
    (:) <$> letterChar <*> many alphaNumChar
    -- `(:) :: Char -> String -> String, and
    -- `letterChar :: Parser Char`, so
    -- `(:) <$> letterChar :: Parser (String -> String)
  notReserved :: String -> Parser String
  notReserved x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x
                       ++ " cannot be an identifier"
                  else return x

phrase :: Parser String -- | does not accept the empty string
phrase = concat . intersperse " " <$> some identifier

thisMany :: Int -> Char -> Parser ()
thisMany n c = string (replicate n c) <* notFollowedBy (char c)
               >> return ()
