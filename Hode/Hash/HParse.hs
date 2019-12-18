-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Hash.HParse (
    pRel  -- ^ Parser PRel
  , _pRel -- ^ Parser PRel
  , pTerm -- ^ Parser PRel
  , pHash -- ^ Level -> Parser (PRel -> PRel -> Either String PRel)
  , pDiff -- ^ Level -> Parser (PRel -> PRel -> Either String PRel)
  , pAnd  -- ^ Level -> Parser (PRel -> PRel -> Either String PRel)
  , pOr   -- ^ Level -> Parser (PRel -> PRel -> Either String PRel)
  , pAbsentMember  -- ^ Parser PRel
  , pPExpr         -- ^ Parser PExpr
  , pReach         -- ^ Parser PExpr
  , pTransRight    -- ^ Parser PExpr
  , pTransLeft     -- ^ Parser PExpr
  , pHashExpr      -- ^ Parser PExpr
  , _pHashExpr     -- ^ Parser PExpr
  , pAddr          -- ^ Parser Expr
  , pAddrs         -- ^ Parser PExpr
  , pPhrase        -- ^ Parser PExpr
  , pTplt          -- ^ Parser Expr
  , _pTplt         -- ^ Parser Expr
  , pMap           -- ^ Parser PExpr
  , pEval          -- ^ Parser PExpr
  , pVar           -- ^ Parser PExpr
  , pAny           -- ^ Parser PExpr
  , pIt            -- ^ Parser PExpr
  , hashPhrase     -- ^ Parser String
  , hashIdentifier -- ^ Parser String
  ) where

import           Control.Monad (void)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.List (intersperse)
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import Hode.Hash.EitherExpr
import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Hash.Hash
import Hode.Qseq.QTypes (Var(..))
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.Util.Alternation
import Hode.Util.UParse


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
              <|> PNonRel <$> pPExpr
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

pPExpr :: Parser PExpr
pPExpr = simplifyPExpr <$> ( foldl1 (<|>) $ map try ps ) where
  ps = [ parens pPExpr

       -- the PExpr constructor
       , pAddrs
       , PExpr <$> pAddr
       , pPhrase
       , PExpr <$> pTplt

         -- other constructors
       , pReach
       , pTransLeft
       , pTransRight
       , pMap
       , pMember
       , pAllTplts
       , pInvolves
       , pEval
       , pVar
       , pAny
       , pIt
       , pHashExpr
       ]

pReach :: Parser PExpr
pReach = lexeme ( try $ precisely "/tr" )
         >> PReach <$> _pHashExpr

pTransLeft :: Parser PExpr
pTransLeft = lexeme ( foldr1 (<|>)
                      $ map (try . precisely) ["/trl","/transLeft"] )
             >> ( PTrans SearchLeftward . PEval <$> _pHashExpr )

pTransRight :: Parser PExpr
pTransRight = lexeme ( foldr1 (<|>)
                      $ map (try . precisely) ["/trr","/transRight"] )
             >> ( PTrans SearchRightward . PEval <$> _pHashExpr )

pHashExpr :: Parser PExpr
pHashExpr = lexeme
            ( try ( precisely "/hash"  <|>
                    precisely "/h"    ) )
            >> _pHashExpr

_pHashExpr :: Parser PExpr
_pHashExpr = PRel <$> pRel

pAddr :: Parser Expr
pAddr = lexeme  ( foldr1 (<|>)
                  $ map (try . precisely) ["/addr","/@"] )
        >> Addr . fromIntegral <$> integer

-- | `pAddrs` parses the string "/@s 1-10 100 30-40 101"
-- to be the disjunction (POr) of 22 `Addr`s.
pAddrs :: Parser PExpr
pAddrs = do
  _ <- lexeme $ foldr1 (<|>) $
       map (try . precisely) ["/addrs","/@s"]
  let range :: Parser [Integer]
      range = do min0 <- lexeme integer
                 _    <- lexeme $ char '-'
                 max0 <- lexeme integer
                 return [min0 .. max0]
  as <- some $ lexeme $
        try range <|> ((: []) <$> integer)
  return $ POr $ map (PExpr . Addr . fromIntegral) $ concat as

pPhrase :: Parser PExpr
pPhrase = lexeme $ hashPhrase >>= return . PExpr . Phrase

pTplt :: Parser Expr
pTplt = lexeme ( foldr1 (<|>) $
                 map (try . precisely) ["/tplt","/t"] )
        >> _pTplt

_pTplt :: Parser Expr
_pTplt =
  let blank = Left  <$> pAny
      separator = Right <$> lexeme hashPhrase
  in some (blank <|> separator) >>=
     return . ExprTplt . fmap Phrase . tpltFromEithers

pMap :: Parser PExpr
pMap = lexeme (precisely "/map" <|> precisely "/roles")
       >> PMap . M.fromList <$> some ( lexeme $ parens $ pMbr
                                       <|> pTplt' )
  where
    pTplt', pMbr :: Parser (Role, PExpr)
    pTplt' = do void $ lexeme $ precisely "tplt"
                t <- _pTplt
                return ( RoleInRel' RoleTplt      , PExpr t )
    pMbr   = do i <- lexeme $ fromIntegral <$> integer
                x <- pPExpr
                return ( RoleInRel' $ RoleMember i, x       )

pMember :: Parser PExpr
pMember = lexeme ( foldr1 (<|>)
                 $ map (try . precisely) ["/member","/m"] )
         >> ( PMember <$> _pHashExpr )

pAllTplts :: Parser PExpr
pAllTplts = lexeme ( foldr1 (<|>)
                     $ map (try . precisely)
                     ["/templates","/tplts","/ts"] )
            >> return PTplts

-- | Example: "/i-2 a" finds any `Rel` that involves "a"
-- in either its first or its second level.
pInvolves :: Parser PExpr
pInvolves = do
  foldr1 (<|>) (map (try . string) ["/involves","/i"])
  >> string "-"
  >> PInvolves <$> decimal <*> _pHashExpr

pEval :: Parser PExpr
pEval = lexeme ( foldr1 (<|>)
                 $ map (try . precisely) ["/eval","/e"] )
         >> ( PEval <$> _pHashExpr )

pVar :: Parser PExpr
pVar = do void $ lexeme
            ( foldr1 (<|>)
              $ map (try . precisely) ["/var","/v"] )
          lexeme hashIdentifier >>= return . PVar . VarString

pAny :: Parser PExpr
pAny = lexeme ( foldr1 (<|>)
                $ map (try . precisely) ["/_","/any"] )
       >> return Any

-- | PITFALL: the /it= keyword, like other keywords,
-- cannot be followed by an adjacent alphanumeric character.
pIt :: Parser PExpr
pIt =     (lexeme (precisely "/it=") >> It . Just <$> _pHashExpr)
      <|> (lexeme (precisely "/it")  >> return (It Nothing) )

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

precisely :: String -> Parser String
precisely s = string s <* notFollowedBy alphaNumChar
