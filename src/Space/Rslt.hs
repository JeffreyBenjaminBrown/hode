{-# LANGUAGE ScopedTypeVariables #-}

module Space.Rslt where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.Index.ImgLookup
import Space.Rslt.Index.Positions
import Space.Rslt.RTypes
import Util


data Rslt = Rslt {
    _exprAt    :: Map Addr Expr
  , _addrOf    :: Map Expr Addr
  , _varieties :: Map Addr (ExprCtr, Arity)
  , _rHas      :: Map Addr (Map Role Addr)
  , _rIsIn     :: Map Addr (Set (Role, Addr))
  } deriving (Show, Eq, Ord)

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
  , _rHas = hasMap
  , _rIsIn = foldl addInvertedPosition M.empty
            $ M.toList $ M.map M.toList hasMap
  }

imgLookup :: Rslt -> ImgOfExpr -> Maybe Addr
imgLookup x img = case img of
  ImgOfExpr e -> M.lookup e $ _addrOf x
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a $ _exprAt x

  ImgOfTplt is -> do
    mas <- ifNothings $ map (imgLookup x) is
    M.lookup (Tplt mas) $ _addrOf x

  ImgOfRel is i -> do
    mas <- ifNothings $ map (imgLookup x) is
    ma <- imgLookup x i
    M.lookup (Rel mas ma) $ _addrOf x

exprAt :: Rslt -> Addr -> Maybe Expr
exprAt = flip M.lookup . _exprAt

addrOf :: Rslt -> Expr -> Maybe Addr
addrOf = flip M.lookup . _addrOf

varieties :: Rslt -> Addr -> Maybe (ExprCtr, Arity)
varieties = flip M.lookup . _varieties

rHas :: Rslt -> Addr -> Maybe (Map Role Addr)
rHas = flip M.lookup . _rHas

rIsIn :: Rslt -> Addr -> Maybe (Set (Role,Addr))
rIsIn = flip M.lookup . _rIsIn

rIsIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
rIsIn1 x (r,a) = case M.lookup a $ _rHas x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
