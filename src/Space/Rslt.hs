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
    xExprs     :: Exprs
  , xImgDb     :: Map Expr Addr
  , xVarieties :: Map Addr (ExprCtr, Arity)
  , xHas       :: Map Addr (Map Role Addr)
  , xIsIn      :: Map Addr (Set (Role, Addr))
  } deriving (Show, Eq, Ord)

mkRslt :: Exprs -> Rslt
mkRslt es = let
  (hasMap :: Map Addr (Map Role Addr)) =
    M.filter (not . M.null)
    $ M.map (M.fromList . exprPositions)
    $ es
  in Rslt {
    xExprs = es
  , xImgDb = imgDb es
  , xVarieties = M.map exprVariety es
  , xHas = hasMap
  , xIsIn = foldl addInvertedPosition M.empty
            $ M.toList $ M.map M.toList hasMap
  }

xImgLookup :: Rslt -> ImgOfExpr -> Maybe Addr
xImgLookup x img = case img of
  ImgOfExpr e -> M.lookup e $ xImgDb x
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a $ xExprs x

  ImgOfTplt is -> do
    mas <- ifNothings $ map (xImgLookup x) is
    M.lookup (Tplt mas) $ xImgDb x

  ImgOfRel is i -> do
    mas <- ifNothings $ map (xImgLookup x) is
    ma <- xImgLookup x i
    M.lookup (Rel mas ma) $ xImgDb x

isIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
isIn1 x (r,a) = case M.lookup a $ xHas x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
