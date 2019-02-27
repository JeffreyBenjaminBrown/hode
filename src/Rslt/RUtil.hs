{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RUtil where

import qualified Data.Map       as M
import qualified Data.Set       as S

import Rslt.RTypes
import Util.Misc


-- | = For `Expr`s

depth :: Expr -> Int
depth (Phrase _)     = 0
depth (Addr _) = 0
depth (Rel mems _) = 1 + maximum (map depth mems)
depth (Tplt _)  = 0 -- ^ TODO ? consider Tplts with non-Phrase members
depth (Par sis _)  = 1 + maximum (map (depth . snd) sis)


-- | for `RefExpr`s

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Phrase'  _) = (PhraseCtr, 0)
refExprVariety e@(Tplt'  _) = (TpltCtr, refExprArity e)
refExprVariety e@(Rel' _ _) = (RelCtr , refExprArity e)
refExprVariety e@(Par' _ _) = (ParCtr , refExprArity e)

refExprArity :: RefExpr -> Arity
refExprArity (Phrase' _)  = 0
refExprArity (Rel' x _) = length x
refExprArity (Tplt' x)  = length x - 1
refExprArity (Par' x _) = length x


-- | = for `Rslt`s

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _addrToRefExpr
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)
