-- for the Variant library
{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

import Data.Functor.Foldable

import Hode.Rslt.RTypes
import Hode.Rslt.Show.Util (Parens(..))


-- | Takes whatever is attached to a `Fix EprFWith`,
-- and attaches as well a depth and a `Parens`.
parenExprAtDepth'
  :: forall att. Int -> Fix (ExprFWith att)
  -> Fix (ExprFWith (att,(Int,Parens)))
parenExprAtDepth' maxDepth = g where
  g (Fix (EFW (att, x))) = Fix $ EFW $ f att x

  f :: att -> ExprF (Fix (ExprFWith att))
    -> ( (att,(Int,Parens)),
         ExprF (Fix (ExprFWith (att,(Int,Parens)))))
  f att (AddrF a) = ( (att, (0, Naked) )
                  , AddrF a)
  f att (PhraseF p) = ( (att, (0, Naked) )
                    , PhraseF p)

  f att (ExprTpltF js) =
    ( (att,(0,InParens))
    , ExprTpltF $
      fmap (parenExprAtDepth' maxDepth) js )

  f att (ExprRelF (Rel ms t)) = let
    ms' :: [Fix (ExprFWith (att, (Int, Parens)))]
    ms' = map (parenExprAtDepth' maxDepth) ms
    d = (+1) $ maximum $ map h ms'
      where h = nakedDepth .
                \(Fix (EFW ((_,(i,p)),_))) -> (i,p)
            nakedDepth :: (Int,Parens) -> Int
            nakedDepth (_,InParens) = 0
            nakedDepth (i,_) = i

    in ( (att, ( d
               , if d >= maxDepth
                 then InParens else Naked ) )
       , ExprRelF $ Rel ms' $
         parenExprAtDepth' maxDepth t )
