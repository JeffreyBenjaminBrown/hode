-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Hash.Parse (
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
  , pAddrs         -- ^ Parser PExpr
  , pPhrase        -- ^ Parser PExpr
  , pTplt          -- ^ Parser Expr
  , _pTplt         -- ^ Parser Expr
  , pMap           -- ^ Parser PExpr
  , pMember        -- ^ Parser PExpr
  , pAllTplts      -- ^ Parser PExpr
  , pInvolves      -- ^ Parser PExpr
  , pEval          -- ^ Parser PExpr
  , pVar           -- ^ Parser PExpr
  , pAny           -- ^ Parser PExpr
  , pIt            -- ^ Parser PExpr
  , hashPhrase     -- ^ Parser String
  , hashWord -- ^ Parser String
  ) where

import           Control.Monad (void)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.List (intersperse)
import           GHC.Unicode (isSpace)
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Hode.Hash.EitherExpr
import           Hode.Hash.Hash
import qualified Hode.Hash.Parse.Keywords as KW
import           Hode.Hash.Parse.Util (pThisMany)
import           Hode.Hash.Types
import           Hode.Hash.Util
import           Hode.Qseq.Types (Var(..))
import           Hode.Rslt.Binary
import           Hode.Rslt.Types
import           Hode.Util.Alternation
import           Hode.Util.Parse


pRel :: Parser PRel
pRel = lexeme $ sc >> simplifyPRel <$> _pRel

_pRel :: Parser PRel
_pRel = eMakeExprParser pTerm
 [ [ EInfixL $ try $ pHash n
   , EInfixL $ try $ pDiff n
   , EInfixL $ try $ pAnd n
   , EInfixL $ try $ pOr n ]
 | n <- [1..8] ]

pTerm :: Parser PRel
pTerm = close <$> parens _pRel
              <|> PNonRel <$> pPExpr
              <|> pAbsentMember

pHash :: Level -> Parser (PRel -> PRel -> Either String PRel)
pHash n = lexeme $ do
  pThisMany n KW.hash
  label <- option ""
    $ hashWord
    <|> parens ( concat . L.intersperse " "
                 <$> some (lexeme hashPhrase) )
  return $ hash n label

pDiff :: Level -> Parser (PRel -> PRel -> Either String PRel)
pDiff n = lexeme $ do
  pThisMany n KW.diff
  return $ \a b -> Right $ PNonRel $ PDiff (PRel a) (PRel b)

pAnd :: Level -> Parser (PRel -> PRel -> Either String PRel)
pAnd n = lexeme $ do
  pThisMany n KW.hAnd
  return $ \a b -> Right $ PNonRel $ PAnd $ map PRel [a,b]

pOr :: Level -> Parser (PRel -> PRel -> Either String PRel)
pOr n = lexeme $ do
  pThisMany n KW.hOr
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

       -- this calls PRel
       , pHashExpr
       ]

pReach :: Parser PExpr
pReach = lexeme ( foldr1 (<|>)
                  $ map (try . nonPrefix . string) ["/tr","/reach"] )
         >> PReach <$> _pHashExpr

pTransLeft :: Parser PExpr
pTransLeft = lexeme ( foldr1 (<|>)
                      $ map (try . nonPrefix . string) ["/trl","/transLeft"] )
             >> ( PTrans SearchLeftward . PEval <$> _pHashExpr )

pTransRight :: Parser PExpr
pTransRight = lexeme ( foldr1 (<|>)
                      $ map (try . nonPrefix . string) ["/trr","/transRight"] )
             >> ( PTrans SearchRightward . PEval <$> _pHashExpr )

-- | Anywhere that a hash expression could be parsed,
-- it can optionally be preceded by a keyword like "/hash".
-- TODO ? PITFALL: I'm not sure this is actually ever needed.
pHashExpr :: Parser PExpr
pHashExpr = lexeme ( foldr1 (<|>) $ map (try . nonPrefix . string)
                     ["/hash","/h"] )
            >> _pHashExpr

_pHashExpr :: Parser PExpr
_pHashExpr = PRel <$> pRel

-- | `pAddrs` parses the string "/@s 1-10 100 30-40 101"
-- to be the disjunction (POr) of 22 `Addr`s.
pAddrs :: Parser PExpr
pAddrs = do
  _ <- lexeme $ pThisMany 1 KW.addrs
  let range :: Parser [Integer]
      range = do min0 <- lexeme integer
                 _    <- lexeme $ char '-'
                 max0 <- lexeme integer
                 return [min0 .. max0]
  as <- some $ lexeme $
        try range <|> ((: []) <$> integer)
  return $ POr $ map (PExpr . ExprAddr . fromIntegral) $ concat as

pPhrase :: Parser PExpr
pPhrase = lexeme $ hashPhrase >>= return . PExpr . Phrase

pTplt :: Parser Expr
pTplt = lexeme ( foldr1 (<|>) $
                 map (try . nonPrefix . string) ["/t","/tplt","/template"] )
        >> _pTplt

_pTplt :: Parser Expr
_pTplt =
  let blank     = Left  <$> pAny
      separator = Right <$> lexeme hashPhrase
  in some (blank <|> separator) >>=
     return . ExprTplt . fmap Phrase . tpltFromEithers

pMap :: Parser PExpr
pMap = lexeme ( foldr1 (<|>) $
                map (nonPrefix . try . string) ["/map", "/roles"] )
       >> PMap . M.fromList <$> some ( lexeme $ parens $ pMbr
                                       <|> pTplt' )
  where
    pTplt', pMbr :: Parser (Role, PExpr)
    pTplt' = do void $ lexeme $ nonPrefix $ string "tplt"
                t <- _pTplt
                return ( RoleInRel' $ RoleTplt    , PExpr t )
    pMbr   = do i <- lexeme $ fromIntegral <$> integer
                x <- pPExpr
                return ( RoleInRel' $ RoleMember i, x       )

pMember :: Parser PExpr
pMember = lexeme ( foldr1 (<|>)
                 $ map (try . nonPrefix . string) ["/m","/member"] )
          >> ( PMember <$> _pHashExpr )

pAllTplts :: Parser PExpr
pAllTplts = lexeme ( foldr1 (<|>)
                     $ map (try . nonPrefix . string)
                     ["/ts","/tplts","/templates"] )
            >> return PTplts

-- | Example: "/i-2 a" finds any `Rel` that involves "a"
-- in either its first or its second level.
pInvolves :: Parser PExpr
pInvolves = do
  foldr1 (<|>) (map (try . string) ["/i","/involves"])
  >> string "-"
  >> PInvolves <$> decimal <*> _pHashExpr

pEval :: Parser PExpr
pEval = lexeme ( foldr1 (<|>)
                 $ map (try . nonPrefix . string) ["/eval","/e"] )
         >> ( PEval <$> _pHashExpr )

-- | PITFALL: the /it= keyword, like other keywords,
-- cannot be followed by an adjacent alphanumeric character.
pIt :: Parser PExpr
pIt = let
  keyword = lexeme . nonPrefix . string
  in (keyword "/it=" >> It . Just <$> _pHashExpr) <|>
     (keyword "/it"  >> return (It Nothing)     )

pVar :: Parser PExpr
pVar = do void $ lexeme
            ( foldr1 (<|>)
              $ map (try . nonPrefix . string) ["/var","/v"] )
          lexeme hashWord >>= return . PVar . VarString

pAny :: Parser PExpr
pAny = lexeme ( foldr1 (<|>)
                $ map (try . nonPrefix . string) ["/_","/any"] )
       >> return Any

hashPhrase :: Parser String
hashPhrase =
  concat . intersperse " "
  <$> some (lexeme quoted <|> hashWord)
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
  quotedCharacter =
    fmap return nonEscape -- `fmap return` wraps the `Char` into a `String`
    <|> escape

  escape :: Parser String
  escape = do
      void $ char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return [c]

  nonEscape :: Parser Char
  nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

-- | Can contain anything but space, (") or parens,
-- and it cannot start with / or #. (`hashPhrase` is more permissive.)
hashWord :: Parser String
hashWord = lexeme $ do
  let notSpaceOrThese :: String -> Parser Char
      notSpaceOrThese these =
        satisfy $ \c -> not (isSpace c) &&
                        not (elem c these)
  h <-           notSpaceOrThese "#/\"()"
  (h:) <$> many (notSpaceOrThese   "\"()")
