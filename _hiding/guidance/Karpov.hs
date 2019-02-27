-- by Mark Karpov, with trivial modifications by jbb
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

{-# LANGUAGE ScopedTypeVariables #-}

module Parse where

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr
import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data RBinOp
  = Greater
  | Less
  deriving (Show)

data AExpr
  = Var String
  | IntConst Integer
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  deriving (Show)

type Parser = Parsec Void String


-- | == Lexer

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


-- | == Parser

-- | The top-level parser. (The language is called "while".)
whileParser :: Parser Stmt
whileParser = between sc eof stmt

stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi where
  -- if there's only one stmt, return it without using ‘Seq’
  f l = if length l == 1
        then head l else Seq l

stmt' :: Parser Stmt
stmt' = ifStmt
  <|> whileStmt
  <|> skipStmt
  <|> assignStmt
  <|> parens stmt

ifStmt :: Parser Stmt
ifStmt = do
  reservedWord "if"
  cond  <- bExpr
  reservedWord "then"
  stmt1 <- stmt
  reservedWord "else"
  stmt2 <- stmt
  return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
  reservedWord "while"
  cond <- bExpr
  reservedWord "do"
  stmt1 <- stmt
  return $ While cond stmt1

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  void $ symbol ":="
  expr <- aExpr
  return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = Skip <$ reservedWord "skip"


-- | = Parsing expressions

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [ Prefix $ Neg              <$ symbol "-" ]
  , [ InfixL $ ABinary Multiply <$ symbol "*"
    , InfixL $ ABinary Divide   <$ symbol "/" ]
  , [ InfixL $ ABinary Add      <$ symbol "+"
    , InfixL $ ABinary Subtract <$ symbol "-" ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [ Prefix $ Not         <$ reservedWord "not" ]
  , [ InfixL $ BBinary And <$ reservedWord "and"
    , InfixL $ BBinary Or  <$ reservedWord "or" ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> BoolConst True  <$ reservedWord "true"
  <|> BoolConst False <$ reservedWord "false"
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return $ RBinary op a1 a2

relation :: Parser RBinOp
relation = Greater  <$ symbol ">"
           <|> Less <$ symbol "<"

main :: IO ()
main = do
  input <- getContents
  parseTest whileParser input
