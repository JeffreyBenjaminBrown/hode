-- PITFALL: I stopped maintaining this library after commit
-- eab7a7562873a09374393294fa0575182a48071e
-- (when I started making `Tplt a` more complicated --
-- it used to be a synonym for `[a]`).

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.JustInCase (
  exprFWithDepth -- ^ Fix (ExprFWith b) -> Fix (ExprFWith (Int,b))
  ) where

import Data.Functor.Foldable

import Hode.Rslt.Types


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

test_exprFWithDepth :: Test
test_exprFWithDepth = TestCase $ do
  let e :: Int -> Expr
      e 0 = Addr 0
      e n = ExprRel $ Rel [e $ n-1] $ e 0
      fe :: Int -> Fix (ExprFWith (Int,()))
      fe 0 = Fix $ EFW ( (0,()), AddrF 0 )
      fe n = ( Fix $
              EFW ( (n,()), ExprRelF $ Rel [fe $ n-1] $ fe 0 ) )
  assertBool "1" $
    ( exprFWithDepth $ toExprWith () $ e 1 ) == fe 1
  assertBool "1" $
    ( exprFWithDepth $ toExprWith () $ e 2 ) == fe 2

