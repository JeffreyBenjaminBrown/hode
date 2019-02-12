-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HParse where

import           Control.Monad (void)
import           Control.Monad.Combinators.Expr
import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Hash.HTypes
import           Util.Parse


expr :: Parser PRel
expr = lexeme $ sc >> _expr

-- TODO : how to use (a -> a -> Either _ a)
-- where makeExprParser wants (a -> a -> a)?
_expr :: Parser PRel
_expr = makeExprParser term
 [ [ InfixL $ try $ pHash n
   ] | n <- [1..8] ]

term :: Parser PRel
term = Leaf <$> identifier
       <|> close <$> parens _expr
       <|> absent

pHash :: Level -> Parser (PRel -> PRel -> Either String PRel)
pHash n = lexeme $ do
  thisMany n '#'
  label <- option "" $ identifier <|> parens phrase
  return $ hash n label

absent :: Parser PRel
absent = const Absent <$> f <?> "Intended to \"find\" nothing."
  where f = lookAhead $ const () <$> satisfy (== '#') <|> eof
  -- If it finds #, this is an absent leftmost member.
  -- If it finds eof, this is an absent rightmost member.
  -- (Parens get consumed in pairs in an outer (earlier) context.)
