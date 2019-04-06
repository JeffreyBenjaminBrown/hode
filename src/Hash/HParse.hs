-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HParse where

import           Control.Monad (void)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.List (intersperse)
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
  label <- option ""
    $ hashIdentifier
    <|> parens ( concat . L.intersperse " "
                 <$> some (lexeme hashPhrase) )
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
pExpr = simplifyPExpr <$> ( foldl1 (<|>) ps ) where
  ps = [ parens pExpr

       -- the PExpr constructor
       , PExpr <$> pAddr
       , pPhrase
       , PExpr <$> pTplt

         -- other constructors
       , pMap
       , pEval
       , pVar
       , pAny
       , pIt
       , pHashExpr
       ]

pHashExpr :: Parser PExpr
pHashExpr = lexeme ( foldr1 (<|>)
                  $ map (try . string) ["/hash","/h"] )
         >> _pHashExpr

-- | PITFALL: This used to be part of pHashExpr; there was no word
-- for parsing a Hash expression without a leading /hash keyword.
-- Not requiring that keyword is nice for the user,
-- might (I don't know) cause headaches for the coder.
_pHashExpr :: Parser PExpr
_pHashExpr = PRel <$> pRel

pAddr :: Parser Expr
pAddr = lexeme  ( foldr1 (<|>)
                  $ map (try . string) ["/addr","/@"] )
        >> Addr . fromIntegral <$> integer

pPhrase :: Parser PExpr
pPhrase = lexeme $ hashPhrase >>= return . PExpr . Phrase

pTplt :: Parser Expr
pTplt = lexeme  ( foldr1 (<|>)
                  $ map (try . string) ["/tplt","/t"] )
        >> _pTplt

_pTplt :: Parser Expr
_pTplt = lexeme $ ExprTplt . map Phrase
         <$> some (hashIdentifier <|> parens hashPhrase)

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
pEval = lexeme ( foldr1 (<|>)
                 $ map (try . string) ["/eval","/e"] )
         >> ( PEval
              <$> _pHashExpr )

pVar :: Parser PExpr
pVar = do void $ lexeme
            ( foldr1 (<|>)
              $ map (try . string) ["/var","/v"] )
          lexeme hashIdentifier >>= return . PVar

pAny :: Parser PExpr
pAny = lexeme ( foldr1 (<|>)
                $ map (try . string) ["/_","/any"] )
       >> return Any

pIt :: Parser PExpr
pIt = id  (lexeme (string "/it=") >> It . Just <$> pExpr)
      <|> (lexeme (string "/it")  >> return (It Nothing))


-- | like `phrase`, but includes every character that's not special
-- Hash syntax.
hashPhrase :: Parser String
hashPhrase =
  quoted
  <|> concat . intersperse " " <$> some hashIdentifier
 where

  quoted :: Parser String
  -- A quoted string can have any character in it.
  -- Some of those might need escaping (e.g. \" and \\).
  -- This comes nearly verbatim from
  -- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
  quoted = do void $ char '"'
              strings <- many quotedCharacter
              void $ char '"'
              return $ concat strings

  quotedCharacter :: Parser String
  quotedCharacter = fmap return nonEscape <|> escape

  escape :: Parser String
  escape = do
      void $ char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return [c]

  nonEscape :: Parser Char
  nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

-- | Every character that isn't special Hash syntax.
hashIdentifier :: Parser String
hashIdentifier = lexeme $ some $ foldr1 (<|>)
  ( alphaNumChar : map char
    [ '!','@','%','^','*','+','=','-','`','~','_','[',']'
    ,'{','}',':',';','<','>','?',',','.' ] )
