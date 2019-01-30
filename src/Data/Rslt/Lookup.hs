{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Lookup where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.Index
import Data.Rslt.RTypes
import Util


-- | = Search

lookup :: Rslt -> ImgOfExpr -> Maybe Addr
lookup x img = case img of
  ImgOfExpr e -> M.lookup e $ _addrOf x
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a $ _exprAt x

  ImgOfTplt is -> do
    mas <- ifNothings $ map (lookup x) is
    M.lookup (Tplt mas) $ _addrOf x

  ImgOfRel is i -> do
    mas <- ifNothings $ map (lookup x) is
    ma <- lookup x i
    M.lookup (Rel mas ma) $ _addrOf x

exprAt :: Rslt -> Addr -> Maybe Expr
exprAt = flip M.lookup . _exprAt

addrOf :: Rslt -> Expr -> Maybe Addr
addrOf = flip M.lookup . _addrOf

variety :: Rslt -> Addr -> Maybe (ExprCtr, Arity)
variety = flip M.lookup . _variety

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Maybe (Map Role Addr)
has r a = do exprAt r a
             maybe (Just M.empty) Just $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Maybe (Set (Role,Addr))
isIn r a = do
  exprAt r a
  maybe (Just S.empty) Just $ M.lookup a $ _isIn r

-- | `isIn1 r (role,a)` finds the expression that occupies
-- role in a.
isIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
isIn1 x (r,a) = case M.lookup a $ _has x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
