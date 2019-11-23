-- | PITFALL: Retain for illustrative purposes.
-- This code is mostly supplanted by the even more complex
-- `eParenShowColor`.

{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Rslt.Show (
    eShow           -- ^        Rslt -> Expr -> Either String String
  , eParenShowExpr  -- ^ Int -> Rslt -> Expr -> Either String String
  , eParenShowAddr
    -- ^ Int       -- ^ maximum depth before parenthesizing an `Expr`
    -- -> Rslt
    -- -> Set Addr -- ^ these `Addr`s will be shown as `Addr`s
    --             --   rather than expanded into text
    -- -> Addr     -- ^ what to show
    -- -> Either String String
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Util
import Hode.Util.Alternation
import Hode.Util.Misc


-- | `eShow` provides the simplest way to show an `Expr`.
-- It prefaces each joint in an `n`th-order relationships with `n` '#' marks.
eShow :: Rslt -> Expr -> Either String String
eShow r = prefixLeft "eShow: " . para f where
  f :: Base Expr (Expr, Either String String) -> Either String String

  f e@(AddrF _) =
    prefixLeft ", called on Addr: "
    $ unAddr r (embed $ fmap fst e)
    >>= eShow r

  f (PhraseF w) = Right w

  f (ExprTpltF pairs) =
    prefixLeft ", called on ExprTplt: " $ do
      Tplt ma bs mc <- ifLefts $ fmap snd pairs
      Right $ L.intercalate " " ( maybeToList ma ++
                                  zip' (repeat "_") bs ++
                                  maybeToList mc )

  f relf@(ExprRelF (Rel ms (ExprTplt t0,_))) =
    -- The recursive argument (second member of the pair) for the Tplt
    -- is unused -- we don't need to show the whole Tplt, just its parts --
    -- and therefore not computed.
    prefixLeft ", called on ExprRel: " $ do
    t1 :: Tplt String <- ifLefts $ fmap (eShow r) t0
    mss :: [String] <- ifLefts $ map snd ms
    let rel :: Expr = embed $ fmap fst relf
        Tplt ma bs mc :: Tplt String =
          fmap (hash $ depth rel) t1
        ss :: [String] =
          maybeToList ma ++ zip' mss bs ++ maybeToList mc
    Right $ L.intercalate " " ss

  f (ExprRelF (Rel ms (a@(Addr _), _))) =
    prefixLeft ", called on Rel: " $ do
    tpltExpr <- unAddr r a
    eShow r $ ExprRel $ Rel (map fst ms) tpltExpr

  f x@(ExprRelF _) =
    Left $ ": ExprRel with non-Tplt for Tplt: "
    ++ show (embed $ fmap fst x)


-- | = `eParenShowExpr` makes nested `Expr`s easier to read.
-- It takes a `maxDepth` parameter,
-- wraps relationships of order `maxDepth` or higher in parentheses,
-- and then restarts the count. For instance,
-- if `eShow e == "a # b ## c # d ### e # f ## g # h"`,
-- then `eParenShow 2 e == "(a # b ## c # d) # (e # f ## g # h)"`,

eParenShowExpr
  :: Int -- ^ maximum depth before parenthesizing an `Expr`
  -> Rslt -> Expr -> Either String String
eParenShowExpr maxDepth r e =
  prefixLeft "eParenShowExpr:" $ do
  x :: Fix (ExprFWith ((), (Int, Parens))) <-
    parenExprAtDepth maxDepth . toExprWith ()
    <$> unAddrRec r e
  eParenShowInner (const Nothing) x

-- | `eParenShowAddr` is like `eParenShowExpr`.
-- The type signature explains their differences.
eParenShowAddr
  :: Int      -- ^ maximum depth before parenthesizing an `Expr`
  -> Rslt
  -> Set Addr -- ^ these `Addr`s will be shown as `Addr`s
              --   rather than expanded into text
  -> Addr     -- ^ what to show
  -> Either String String
eParenShowAddr maxDepth r as a0 =
  prefixLeft "eParenShowAddr:" $ do
  let showAsAddr :: Addr -> Maybe String
      showAsAddr a = if S.member a as
                     then Just $ "@" ++ show a
                     else Nothing
  fea :: Fix (ExprFWith (Addr,(Int,Parens))) <-
    parenExprAtDepth maxDepth <$> addrToExprWith r a0
  eParenShowInner showAsAddr fea

eParenShowInner :: forall a
  .  (a -> Maybe String)
  -> Fix (ExprFWith (a, (Int, Parens)))
  -> Either String String
eParenShowInner shortCircuit ef0 =
  prefixLeft "eParenShow: " $ fo ef0 where

  shortOrG :: a -> Int
           -> ExprF (Fix (ExprFWith (a, (Int, Parens))))
           -> Either String String
  shortOrG a i e =
    case shortCircuit a of Just s -> Right s -- don't recurse
                           Nothing -> g (i,e)

  -- `fo` is like `f`, but for the "outermost" `Expr`..
  -- Even if that top-level `Expr` has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (a,(Int,Parens))) -> Either String String
  fo (Fix (EFW ((a,(i,_)),e)))       =           shortOrG a i e

  f :: Fix (ExprFWith (a,(Int,Parens))) -> Either String String
  f (Fix (EFW ((a,(i,InParens)),e))) = paren <$> shortOrG a i e
  f (Fix (EFW ((a,(i,Naked))   ,e))) =           shortOrG a i e

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  -- That is why the first argument to `g` has a complex type signature.
  g :: (Int, ExprF (Fix (ExprFWith (a,(Int,Parens)))))
    -> Either String String
  g (_, AddrF _) = Left "Should this be possible? Currently it isn't."
  g (_, PhraseF p) = Right p

  g (_, ExprTpltF t) =
    prefixLeft "g of Tplt: " $ do
    Tplt ml js mr :: Tplt String <-
      ifLefts $ fmap f t
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
