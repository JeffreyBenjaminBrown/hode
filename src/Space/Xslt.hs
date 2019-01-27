{-# LANGUAGE ScopedTypeVariables #-}

module Space.Xslt where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.Index.ImgLookup
import Space.Rslt.Index.Positions
import Space.Rslt.RTypes


data Xslt = Xslt {
    xExprs     :: Exprs
  , xImgDb     :: Map Expr Addr
  , xVarieties :: Map Addr (ExprCtr, Arity)
  , xHas       :: Map Addr (Map Role Addr)
  , xIsIn      :: Map Addr (Set (Role, Addr))
  } deriving (Show, Eq, Ord)

mkRslt :: Exprs -> Xslt
mkRslt es = let
  (hasMap :: Map Addr (Map Role Addr)) =
    M.filter (not . M.null)
    $ M.map (M.fromList . exprPositions)
    $ es
  in Xslt {
    xExprs = es
  , xImgDb = imgDb es
  , xVarieties = M.map exprVariety es
  , xHas = hasMap
  , xIsIn = foldl addInvertedPosition M.empty
            $ M.toList $ M.map M.toList hasMap
  }
