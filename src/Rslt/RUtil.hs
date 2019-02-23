{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RUtil where

import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes (Var)
import Rslt.RTypes
import Util.Misc


-- | = For `Expr`s

depth :: Expr -> Int
depth (Word _)     = 0
depth (Addr _) = 0
depth (Rel mems _) = 1 + maximum (map depth mems)
depth (Tplt _)  = 0 -- ^ TODO ? consider Tplts with non-Word members
depth (Par sis _)  = 1 + maximum (map (depth . snd) sis)


-- | for `RefExpr`s

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Word'  _) = (WordCtr, 0)
refExprVariety e@(Tplt'  _) = (TpltCtr, refExprArity e)
refExprVariety e@(Rel' _ _) = (RelCtr , refExprArity e)
refExprVariety e@(Par' _ _) = (ParCtr , refExprArity e)

refExprArity :: RefExpr -> Arity
refExprArity (Word' _)  = 0
refExprArity (Rel' x _) = length x
refExprArity (Tplt' x)  = length x - 1
refExprArity (Par' x _) = length x


-- | = for `Rslt`s

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _addrToRefExpr
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)


-- | = for Hash

hExprToExpr :: HExpr -> Either String Expr
hExprToExpr (HExpr e) = Right e
hExprToExpr h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr and (soon)HMap constructors can be converted."

hVars :: HExpr -> Set Var
hVars (HMap m)    = S.unions $ map hVars $ M.elems m
hVars (HEval m _) = hVars m
hVars (HVar v)    = S.singleton v
hVars (HExpr _)   = S.empty
hVars (HDiff h i) = S.union (hVars h) (hVars i)
hVars (HAnd hs)   = S.unions $ map hVars hs
hVars (HOr hs)    = S.unions $ map hVars hs
