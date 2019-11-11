{-# LANGUAGE
ScopedTypeVariables,
TupleSections,
ViewPatterns #-}

module Hode.Rslt.ShowColor
  ( eParenShowColor -- ^ Int -> Rslt -> Expr -> Either String ColorString
  ) where

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.List as L

import Hode.Brick
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Util hiding (hash)
import Hode.Util.Misc
import Hode.Util.Alternation


-- | This function is very similar to `eParenShow`,
-- which is simpler.

eParenShowColor :: Int -> Rslt -> Expr -> Either String ColorString
eParenShowColor maxDepth r e0 =
  prefixLeft "eParenShowColor: " $
  unAddrRec r e0 >>=
  fo . parenExprAtDepth maxDepth . toExprWith () where

  f :: Fix (ExprFWith (Int,Parens)) -> Either String ColorString
  f (Fix (EFW ((i,InParens),e))) = attrParen <$> g (i,e)
  f (Fix (EFW ((i,Naked)   ,e))) =               g (i,e)

  -- `fo` = `f, outermost`. For the top-level Expr,
  -- even if it has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (Int,Parens)) -> Either String ColorString
  fo (Fix (EFW ((i,_),e))) = g (i,e)

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  -- That is why the first argument to `g` has a complex type signature.
  g :: (Int, ExprF (Fix (ExprFWith (Int,Parens))))
    -> Either String ColorString
  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right [(p,TextColor)]

  g (_, ExprTpltF t) =
    prefixLeft "g of Tplt: " $ do
    Tplt ml js mr :: Tplt ColorString <-
      ifLefts $ fmap f t
    let mss :: Maybe ColorString -> ColorString
        mss Nothing  = emptyColorString
        mss (Just a) = a
    Right $ attrStrip $ concat $
      L.intersperse space $
      L.intersperse blank $
      ( [mss ml] ++ js ++ [mss mr] )

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF t))))) =
    prefixLeft "g of Rel: " $ do
    ms1 :: [ColorString] <- ifLefts $ map f ms0
    Tplt ml js mr :: Tplt ColorString <-
      ifLefts $ fmap (hash n) <$> fmap f t
    Right $ concat $ L.intersperse space $
      maybeToList ml ++
      zip' ms1 js ++
      maybeToList mr

  g (_, ExprRelF (Rel _ _)) = Left $
    "g given a Rel with a non-Tplt in the Tplt position."

blank, space, emptyColorString :: ColorString
blank = [("_", TextColor)]
space = [(" ", TextColor)]
emptyColorString = [("", TextColor)]

hash :: Int -> ColorString -> ColorString
hash k s = (replicate k '#', SepColor) : s
