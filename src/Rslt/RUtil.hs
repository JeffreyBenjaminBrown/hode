{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RUtil where

import           Data.Functor.Foldable
import           Data.Either hiding (lefts)
import qualified Data.Map       as M
import qualified Data.Set       as S

import Rslt.RTypes
import Util.Misc


-- | = Rel

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


-- | for `RefExpr`s

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Phrase'  _) = (PhraseCtr, 0)
refExprVariety e@(Tplt'  _)   = (TpltCtr, refExprArity e)
refExprVariety e@(Rel' _)     = (RelCtr , refExprArity e)

refExprArity :: RefExpr -> Arity
refExprArity (Phrase' _)      = 0
refExprArity (Rel' (Rel x _)) = length x
refExprArity (Tplt' x)        = length x - 1


-- | = for `Rslt`s

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _addrToRefExpr
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)
