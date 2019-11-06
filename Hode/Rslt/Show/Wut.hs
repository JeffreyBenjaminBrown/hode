-- | PITFALL: Retain for illustrative purposes.
-- This code is mostly supplanted by the even more complex
-- `eParenShow'`.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.Wut (
    Parens(..)
  , eParenShow       -- ^ Int -> Rslt -> Expr -> Either String String
  , parenExprAtDepth -- ^ Int -> Fix (ExprFWith ())
                     --       -> Fix (ExprFWith (Int,Parens))
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Util
import Hode.Util.Misc
import Hode.Util.Alternation


data Parens = InParens | Naked
  deriving (Show, Eq, Ord)

eParenShow :: Int -> Rslt -> Expr -> Either String String
eParenShow maxDepth r e0 =
  prefixLeft "eParenShow: " $
  unAddrRec r e0 >>=
  fo . parenExprAtDepth maxDepth . toExprWith () where

  paren :: String -> String
  paren s = "(" ++ s ++ ")"

  f :: Fix (ExprFWith (Int,Parens)) -> Either String String
  f (Fix (EFW ((i,InParens),e))) = paren <$> g (i,e)
  f (Fix (EFW ((i,Naked)   ,e))) =           g (i,e)

  -- `fo` = `f, outermost`. For the top-level Expr,
  -- even if it has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (Int,Parens)) -> Either String String
  fo (Fix (EFW ((i,_),e))) = g (i,e)

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  g :: (Int, ExprF (Fix (ExprFWith (Int,Parens))))
    -> Either String String
  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right p

  g (_, ExprTpltF js0) =
    prefixLeft "g of Tplt: " $ do
    Tplt ml js mr :: Tplt String <-
      ifLefts $ fmap f js0
    let mss :: Maybe String -> String
        mss Nothing = ""
        mss (Just a) = a
    Right $ (T.unpack . T.strip . T.pack) $ concat $
      L.intersperse " " $ L.intersperse "_" $
      ( [mss ml] ++ js ++ [mss mr] )

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF t))))) =
    prefixLeft "g of Rel: " $ do
    ms1 :: [String] <- ifLefts $ map f ms0
    Tplt ml js mr :: Tplt String <-
      (hash n <$>) <$> -- Tplt in Either => two fmaps
      ifLefts (fmap f t)
    Right $ concat $ L.intersperse " " $
      maybeToList ml ++ zip' ms1 js ++
      maybeToList mr

  g (_, ExprRelF (Rel _ _)) = Left $
    "g given a Rel with a non-Tplt in the Tplt position."

-- | Attaches a depth (`Int`) and a `Parens` to each `subExpr` of an `Expr`.
-- Anything `InParens` will be shown in parens.
-- The depth of a `Rel` is the maximum `nakedDepth` of its members,
-- where `nakedDepth` is like normal depth,
-- except `InParens` things contribute 0 depth.
    -- TODO ? should that be 1 instead of 0?
-- The depth of any non-`Rel` is 0.
-- A `Rel` of depth > `maxDepth` is `InParens`.
-- `Tplt`s are also `InParens` (which might not be necessary).

parenExprAtDepth :: Int -> Fix (ExprFWith ())
                        -> Fix (ExprFWith (Int,Parens))
parenExprAtDepth maxDepth = g where
  g (Fix (EFW ((), x))) = Fix $ EFW $ f x where

    f ::                ExprF (Fix (ExprFWith ()))
      -> ((Int,Parens), ExprF (Fix (ExprFWith (Int,Parens))))
    f (AddrF a)      =
      ( (0,Naked), AddrF a)
    f (PhraseF p)    =
      ( (0,Naked), PhraseF p)

    f (ExprTpltF js) =
      ( (0,InParens)
      , ExprTpltF $
        fmap (parenExprAtDepth maxDepth) js )
    f (ExprRelF (Rel ms t)) =
      ( (d, if d >= maxDepth then InParens else Naked)
      , ExprRelF $ Rel ms' $
        parenExprAtDepth maxDepth t) where
      ms' = map (parenExprAtDepth maxDepth) ms
      d = (+1) $ maximum $ map h ms' where
        h = nakedDepth . \(Fix (EFW (b,_))) -> b where
          nakedDepth :: (Int,Parens) -> Int
          nakedDepth (_,InParens) = 0
          nakedDepth (i,_) = i
