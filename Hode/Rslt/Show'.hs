{-# LANGUAGE
ScopedTypeVariables,
TupleSections,
ViewPatterns #-}

module Hode.Rslt.Show'
  ( eParenShow' -- ^ Int -> Rslt -> Expr -> Either String AttrString
  , hashUnlessEmptyStartOrEnd' -- ^ Int -> [AttrString] -> [AttrString]
  ) where

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.List as L

import Hode.Brick
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Wut
import Hode.Util.Misc
import Hode.Util.Alternation
import Hode.Util.UParse


eParenShow' :: Int -> Rslt -> Expr -> Either String AttrString
eParenShow' maxDepth r e0 =
  prefixLeft "eParenShow': " $
  unAddrRec r e0 >>=
  fo . parenExprAtDepth maxDepth . toExprWith () where

  f :: Fix (ExprFWith (Int,Parens)) -> Either String AttrString
  f (Fix (EFW ((i,InParens),e))) = attrParen <$> g (i,e)
  f (Fix (EFW ((i,Naked)  ,e))) =                g (i,e)

  -- `fo` = `f, outermost`. For the top-level Expr,
  -- even if it has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (Int,Parens)) -> Either String AttrString
  fo (Fix (EFW ((i,_),e))) = g (i,e)

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  g :: (Int, ExprF (Fix (ExprFWith (Int,Parens))))
    -> Either String AttrString
  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right [(p,textColor)]

  g (_, ExprTpltF t) = do
    Tplt ma bs mc :: Tplt AttrString <- ifLefts $ fmap f t
    Right $ concat $ L.intersperse space
      ( maybeToList ma ++
        zip' (repeat blank) bs ++
        maybeToList mc )

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF t))))) =
    prefixLeft "g of Rel: " $ do
    ms1 :: [AttrString] <- ifLefts $ map f ms0
    Tplt ma bs mc :: Tplt AttrString <-
      ifLefts $ fmap (hash n) <$> fmap f t
    Right $ concat $ L.intersperse space $
      maybeToList ma ++
      zip' ms1 bs ++
      maybeToList mc

  g (_, ExprRelF (Rel _ _)) = Left $
    "g given a Rel with a non-Tplt in the Tplt position."

-- | `hashUnlessEmptyStartOrEnd k js` adds `k` #-marks to every joint
-- in `js`, unless it's first or last and the empty string.
hashUnlessEmptyStartOrEnd' :: Int -> [AttrString] -> [AttrString]
hashUnlessEmptyStartOrEnd' k0 joints = case joints' of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  joints' :: [AttrString] = map maybeParens joints where
    maybeParens :: AttrString -> AttrString
    maybeParens s = let
      s' = concatMap fst s
      in if hasMultipleWords s'
      then [("(",sepColor)] ++ s ++ [(")",sepColor)]
      else s

  hashUnlessEmpty :: Int -> AttrString -> AttrString
  hashUnlessEmpty _ [("",_)] = [("",textColor)]
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [AttrString] -> [AttrString]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss

blank :: AttrString
blank = [("_", textColor)]

space :: AttrString
space = [(" ", textColor)]

hash :: Int -> AttrString -> AttrString
hash k s = (replicate k '#', sepColor) : s
