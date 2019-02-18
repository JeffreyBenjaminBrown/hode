-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HParse where

import           Control.Monad (void)
import qualified Data.Map as M
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char

import           Hash.EitherExpr
import           Hash.Hash
import           Hash.HTypes
import           Hash.HUtil
import           Rslt.RTypes
import           Util.UParse


pRel :: Parser PRel
pRel = lexeme $ sc >> simplifyPRel <$> _pRel

_pRel :: Parser PRel
_pRel = eMakeExprParser pTerm
 [ [ EInfixL $ try $ pHash n
   , EInfixL $ try $ pDiff n
   , EInfixL $ try $ pAnd n
   , EInfixL $ try $ pOr n
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

pDiff :: Level -> Parser (PRel -> PRel -> Either String PRel)
pDiff n = lexeme $ do
  thisMany n '\\'
  return $ \a b -> Right $ PNonRel $ PDiff (PRel a) (PRel b)

pAnd :: Level -> Parser (PRel -> PRel -> Either String PRel)
pAnd n = lexeme $ do
  thisMany n '&'
  return $ \a b -> Right $ PNonRel $ PAnd $ map PRel [a,b]

pOr :: Level -> Parser (PRel -> PRel -> Either String PRel)
pOr n = lexeme $ do
  thisMany n '|'
  return $ \a b -> Right $ PNonRel $ POr $ map PRel [a,b]

pAbsentMember :: Parser PRel
pAbsentMember = const Absent <$> f
                <?> "Intended to \"find\" nothing."
  where f = lookAhead $ const () <$> satisfy (== '#') <|> eof
  -- If it finds #, this is an absent leftmost member.
  -- If it finds eof, this is an absent rightmost member.
  -- (Parens get consumed in pairs in an outer (earlier) context.)


-- | = parse a PExpr

pExpr :: Parser PExpr
pExpr = simplifyPExpr <$>
  ( foldl1 (<|>)
    [ parens pExpr

  -- the PExpr constructor
    , PExpr <$> pAddr
    , pWord -- not really necessary -- could use /hash instead
    , PExpr <$> pTplt

  -- other constructors
    , pMap
    , pEval
    , pVar
    , pAny
    , pIt
    , pPar
    , lexeme (string "/hash") >> PRel <$> pRel
  ] )

pAddr :: Parser Expr
pAddr = lexeme (string "/addr")
        >> Addr . fromIntegral <$> integer

pWord :: Parser PExpr
pWord = lexeme $ phrase >>= return . PExpr . Word

pTplt :: Parser Expr
pTplt = lexeme (string "/tplt") >> _pTplt

_pTplt :: Parser Expr
_pTplt = lexeme $ Tplt . map Word
         <$> some (identifier <|> parens phrase)

pMap :: Parser PExpr
pMap = lexeme (string "/map" <|> string "/roles")
       >> PMap . M.fromList <$> some (lexeme $ parens $ pMbr <|> pTplt')
  where
    pTplt', pMbr :: Parser (Role, PExpr)
    pTplt' = do void $ lexeme $ string "tplt"
                t <- _pTplt
                return ( RoleTplt    , PExpr t )
    pMbr   = do i <- lexeme $ fromIntegral <$> integer
                x <- pExpr
                return ( RoleMember i, x       )

pEval :: Parser PExpr
pEval = id  (lexeme (string "/eval") >> PEval <$> pExpr)

pVar :: Parser PExpr
pVar = do void $ lexeme $ string "/var"
          lexeme identifier >>= return . PVar

pAny :: Parser PExpr
pAny = lexeme (string "_") >> return Any

pIt :: Parser PExpr
pIt = id  (lexeme (string "/it=") >> It . Just <$> pExpr)
      <|> (lexeme (string "/it")  >> return (It Nothing))

pPar :: Parser PExpr
pPar = do
  let maybePhrase :: Parser String
      maybePhrase = try phrase <|> return ""
      unit :: Parser (String,PExpr)
      unit = try $ do p <- maybePhrase
                      e <- pExpr
                      return (p,e)

  void $ lexeme $ string "/par"
  us <- many unit
  ap <- maybePhrase
  return $ PPar us ap
