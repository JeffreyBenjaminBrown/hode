{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RUtil where

import           Control.Lens
import           Data.Functor.Foldable
import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S

import Rslt.RTypes
import Util.Misc


-- | = For Par and Rel

ifLefts_par :: String -> Par (Either String a) -> Either String (Par a)
ifLefts_par errMsg (Par pairs s) = let
  lefts = filter isLeft $ map snd pairs
  impossible = error "ifLefts: impossible."
  in case null lefts of
       True -> Right $ Par (map (_2 %~ fromRight impossible) pairs) s
       False -> Left $ errMsg ++ ": "
                ++ concat (map (fromLeft impossible) lefts)

ifLefts_rel :: String -> Rel (Either String a) -> Either String (Rel a)
ifLefts_rel errMsg (Rel es e) = let
  es' = e : es
  lefts = filter isLeft es'
  impossible = error "ifLefts: impossible."
  in case null lefts of
       True -> let
         es'' = map (fromRight impossible) es'
         in Right $ Rel (tail es'') (head es'')
       False -> Left $ errMsg ++ ": "
                ++ concat (map (fromLeft impossible) lefts)


-- | = For `Expr`s

depth :: Expr -> Int
depth = cata f where
  f :: Base Expr Int -> Int
  f (AddrF _)               = 0
  f (PhraseF _)             = 0
  f (ExprRelF (Rel mems _)) = 1 + maximum mems
  f (ExprTpltF _)           = 0
  f (ExprParF (Par sis _))  = 1 + maximum (map snd sis)


-- | for `RefExpr`s

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Phrase'  _) = (PhraseCtr, 0)
refExprVariety e@(Tplt'  _)   = (TpltCtr, refExprArity e)
refExprVariety e@(Rel' _)     = (RelCtr , refExprArity e)
refExprVariety e@(Par' _)     = (ParCtr , refExprArity e)

refExprArity :: RefExpr -> Arity
refExprArity (Phrase' _)      = 0
refExprArity (Rel' (Rel x _)) = length x
refExprArity (Tplt' x)        = length x - 1
refExprArity (Par' (Par x _)) = length x


-- | = for `Rslt`s

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _addrToRefExpr
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)
