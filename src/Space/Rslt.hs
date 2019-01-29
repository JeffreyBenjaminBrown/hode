{-# LANGUAGE ScopedTypeVariables #-}

module Space.Rslt where

import           Prelude hiding (lookup)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.Index
import Space.Rslt.RTypes
import Util


mkRslt :: Exprs -> Rslt
mkRslt es = let
  (hasMap :: Map Addr (Map Role Addr)) =
    M.filter (not . M.null)
    $ M.map (M.fromList . exprPositions)
    $ es
  in Rslt {
    _exprAt = es
  , _addrOf = imgDb es
  , _varieties = M.map exprVariety es
  , _has = hasMap
  , _isIn = foldl addInvertedPosition M.empty
            $ M.toList $ M.map M.toList hasMap
  }

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

varieties :: Rslt -> Addr -> Maybe (ExprCtr, Arity)
varieties = flip M.lookup . _varieties

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Maybe (Map Role Addr)
has = flip M.lookup . _has

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Maybe (Set (Role,Addr))
isIn = flip M.lookup . _isIn

-- | `isIn1 r (role,a)` finds the expression that occupies
-- role in a.
isIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
isIn1 x (r,a) = case M.lookup a $ _has x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
