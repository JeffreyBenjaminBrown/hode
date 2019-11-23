{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.Util (
    trimString       -- ^         String -> String
  , hash             -- ^ Int ->  String -> String

  , Parens(..)
  , paren            -- ^ String -> String
  , parenExprAtDepth -- ^ Int -> Fix (ExprFWith att)
                      -- -> Fix (ExprFWith (att,(Int,Parens)))
  ) where

import Data.Functor.Foldable
import Data.Text (strip, pack, unpack)

import Hode.Rslt.RTypes


trimString :: String -> String
trimString = unpack . strip . pack

hash :: Int -> String -> String
hash k s = replicate k '#' ++ s


-- | = parenthesizing

data Parens = InParens | Naked
  deriving (Show, Eq, Ord)

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | Attaches a depth (`Int`) and a `Parens` to each `subExpr` of an `Expr`.
-- Anything `InParens` will be shown in parens.
-- The depth of a `Rel` is the maximum `nakedDepth` of its members,
-- where `nakedDepth` is like normal depth,
-- except `InParens` things contribute 0 depth.
    -- TODO ? should that be 1 instead of 0?
-- The depth of any non-`Rel` is 0.
-- A `Rel` of depth > `maxDepth` is `InParens`.
-- `Tplt`s are also `InParens` (which might not be necessary).

parenExprAtDepth
  :: forall att. Int -> Fix (ExprFWith att)
  -> Fix (ExprFWith (att,(Int,Parens)))
parenExprAtDepth maxDepth = g where
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
      fmap (parenExprAtDepth maxDepth) js )

  f att (ExprRelF (Rel ms t)) = let
    ms' :: [Fix (ExprFWith (att, (Int, Parens)))]
    ms' = map (parenExprAtDepth maxDepth) ms
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
         parenExprAtDepth maxDepth t )
