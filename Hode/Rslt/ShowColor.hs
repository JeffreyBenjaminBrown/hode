{-# LANGUAGE
ScopedTypeVariables,
TupleSections,
ViewPatterns #-}

module Hode.Rslt.ShowColor
  ( eParenShowColorExpr  -- ^ Int -> Rslt -> Expr -> Either String ColorString
  , eParenShowColorAddr
    -- ^ Int       -- ^ maximum depth before parenthesizing an `Expr`
    -- -> Rslt
    -- -> Set Addr -- ^ these `Addr`s will be shown as `Addr`s
    --             --   rather than expanded into text
    -- -> Addr     -- ^ what to show
    -- -> Either String ColorString
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import Hode.Brick
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Util hiding (hash)
import Hode.Util.Misc
import Hode.Util.Alternation


-- | This function is very similar to `eParenShowExpr`,
-- which is simpler.
eParenShowColorExpr
  :: Int -- ^ maximum depth before parenthesizing an `Expr`
  -> Rslt -> Expr -> Either String ColorString
eParenShowColorExpr maxDepth r e =
  prefixLeft "eParenShowColorExpr:" $ do
  x :: Fix (ExprFWith ((), (Int, Parens))) <-
    parenExprAtDepth maxDepth . toExprWith ()
    <$> unAddrRec r e
  eParenShowColorInner (const Nothing) x

-- TODO ? factor out code this has in common with `eParenShowInner`
eParenShowColorAddr
  :: Int      -- ^ maximum depth before parenthesizing an `Expr`
  -> Rslt
  -> Set Addr -- ^ these `Addr`s will be shown as `Addr`s
              --   rather than expanded into text
  -> Addr     -- ^ what to show
  -> Either String ColorString
eParenShowColorAddr maxDepth r as a0 =
  prefixLeft "eParenShowColorAddr:" $ do
  let showAsAddr :: Addr -> Maybe ColorString
      showAsAddr a = if S.member a as
                     then Just [("@" ++ show a,SepColor)]
                     else Nothing
  fea :: Fix (ExprFWith (Addr,(Int,Parens))) <-
    parenExprAtDepth maxDepth <$> addrToExprWith r a0
  eParenShowColorInner showAsAddr fea

-- TODO : factor out code this has in common with `eParenShowInner`
eParenShowColorInner :: forall a
  .  (a -> Maybe ColorString)
  -> Fix (ExprFWith (a, (Int, Parens)))
  -> Either String ColorString
eParenShowColorInner shortCircuit ef0 =
  prefixLeft "eParenShowColorInner:" $ fo ef0 where

  shortOrG :: a -> Int
           -> ExprF (Fix (ExprFWith (a, (Int, Parens))))
           -> Either String ColorString
  shortOrG a i e =
    case shortCircuit a of Just s  -> Right s -- don't recurse
                           Nothing -> g (i,e)

  -- `fo` is like `f`, but for the "outermost" `Expr`..
  -- Even if that top-level `Expr` has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (a,(Int,Parens))) -> Either String ColorString
  fo (Fix (EFW ((a,(i,_)),e)))       =               shortOrG a i e

  f :: Fix (ExprFWith (a,(Int,Parens))) -> Either String ColorString
  f (Fix (EFW ((a,(i,InParens)),e))) = colorParen <$> shortOrG a i e
  f (Fix (EFW ((a,(i,Naked))   ,e))) =               shortOrG a i e

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  -- That is why the first argument to `g` has a complex type signature.
  g :: (Int, ExprF (Fix (ExprFWith (a,(Int,Parens)))))
    -> Either String ColorString

  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right [(p,TextColor)]

  g (_, ExprTpltF t) =
    prefixLeft "g of Tplt:" $ do
    Tplt ml js mr :: Tplt ColorString <-
      ifLefts $ fmap f t
    let mss :: Maybe ColorString -> ColorString
        mss Nothing  = emptyColorString
        mss (Just a) = a
    Right $ colorStrip $ concat $
      L.intersperse space $
      L.intersperse blank $
      ( [mss ml] ++ js ++ [mss mr] )

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF t))))) =
    prefixLeft "g of Rel:" $ do
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
blank            = [("_", TextColor)]
space            = [(" ", TextColor)]
emptyColorString = [("" , TextColor)]

hash :: Int -> ColorString -> ColorString
hash k s = (replicate k '#', SepColor) : s
