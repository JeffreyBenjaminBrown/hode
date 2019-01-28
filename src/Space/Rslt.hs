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
  , __rIsIn    :: Map Addr (Set (Role, Addr))
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
  , __rIsIn = foldl addInvertedPosition M.empty
            $ M.toList $ M.map M.toList hasMap
  }

xImgLookup :: Rslt -> ImgOfExpr -> Maybe Addr
xImgLookup x img = case img of
  ImgOfExpr e -> M.lookup e $ _addrOf x
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a $ _exprAt x

  ImgOfTplt is -> do
    mas <- ifNothings $ map (xImgLookup x) is
    M.lookup (Tplt mas) $ _addrOf x

  ImgOfRel is i -> do
    mas <- ifNothings $ map (xImgLookup x) is
    ma <- xImgLookup x i
    M.lookup (Rel mas ma) $ _addrOf x

isIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
isIn1 x (r,a) = case M.lookup a $ _rHas x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
