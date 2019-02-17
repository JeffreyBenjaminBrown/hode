-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HParse where

import           Control.Monad (void)
import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Hash.EitherExpr
import           Hash.Hash
import           Hash.HTypes
import           Rslt.RTypes
import           Util.UParse


pRel :: Parser PRel
pRel = lexeme $ sc >> _pRel

_pRel :: Parser PRel
_pRel = eMakeExprParser pTerm
 [ [ EInfixL $ try $ pHash n
   ] | n <- [1..8] ]

pTerm :: Parser PRel
pTerm = close <$> parens _pRel
        <|> PNonRel <$> pExpr
        <|> pAbsentMember

pHash :: Level -> Parser (PRel -> PRel -> Either String PRel)
pHash n = lexeme $ do
  thisMany n '#'
  label <- option "" $ identifier <|> parens phrase
  return $ hash n label

pAbsentMember :: Parser PRel
pAbsentMember = const Absent <$> f
                <?> "Intended to \"find\" nothing."
  where f = lookAhead $ const () <$> satisfy (== '#') <|> eof
  -- If it finds #, this is an absent leftmost member.
  -- If it finds eof, this is an absent rightmost member.
  -- (Parens get consumed in pairs in an outer (earlier) context.)


-- | = parse a PExpr

pExpr :: Parser PExpr
pExpr = foldl1 (<|>)
  [ parens pExpr
  , lexeme (string "/hash") >> PRel <$> pRel
  , pWord
  , pAny
  , pVar
  , pIt ]

pWord :: Parser PExpr
pWord = lexeme $ phrase >>= return . PExpr . Word

pAny :: Parser PExpr
pAny = lexeme (string "_") >> return Any

pVar :: Parser PExpr
pVar = do lexeme $ string "/var"
          identifier >>= return . PVar

pIt :: Parser PExpr
pIt = id  (lexeme (string "/it=") >> It . Just <$> pExpr)
      <|> (lexeme (string "/it")  >> return (It Nothing))

pAddr :: Parser PExpr
pAddr = lexeme (string "/addr")
        >> PExpr . ExprAddr . fromIntegral <$> integer
