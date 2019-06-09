{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.JustInCase (
  exprFWithDepth -- ^ Fix (ExprFWith b) -> Fix (ExprFWith (Int,b))
  ) where

import Data.Functor.Foldable

import Hode.Rslt.RTypes


-- | This isn't used, but it might be helpful
-- for understanding `parenExprAtDepth`.
exprFWithDepth :: Fix (ExprFWith b) -> Fix (ExprFWith (Int,b))
exprFWithDepth (Fix (EFW x)) =
  Fix . EFW $ f x where
  f :: (     b , ExprF (Fix (ExprFWith      b)))
    -> ((Int,b), ExprF (Fix (ExprFWith (Int,b))))
  f (b, AddrF a)      = ((0,b), AddrF a)
  f (b, PhraseF p)    = ((0,b), PhraseF p)
  f (b, ExprTpltF js) = ((0,b), ExprTpltF $
                                map exprFWithDepth js)
  f (b, ExprRelF (Rel ms t)) =
    let msWithDepth = map exprFWithDepth ms
        maxMemberDepth =
          let g (Fix (EFW ((i,_),_))) = i
          in if null msWithDepth then 0
             else maximum $ map g msWithDepth
    in ( ( 1+maxMemberDepth, b)
       , ExprRelF $ Rel msWithDepth $ exprFWithDepth t)
