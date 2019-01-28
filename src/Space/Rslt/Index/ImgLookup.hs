module Space.Rslt.Index.ImgLookup where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.RTypes
import Util


exprVariety :: Expr -> (ExprCtr, Arity)
exprVariety   (Word  _) = (Word', 0)
exprVariety e@(Tplt  _) = (Tplt', arity e)
exprVariety e@(Rel _ _) = (Rel' , arity e)
exprVariety e@(Par _ _) = (Par' , arity e)

imgDb :: Exprs -> Map Expr Addr
imgDb = M.fromList . catMaybes . map f . M.toList where
  f (addr, expr) = case expr of
    Par _ _ -> Nothing
    _       -> Just (expr, addr)
