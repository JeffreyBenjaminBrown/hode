{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.RUtil (
    LeftStrings(..)
  , replaceNth_tplt -- ^ a -> Int -> Tplt -> Either String Tplt
  , tpltToList      -- ^ Tplt a -> [a]
  , toExprWith      -- ^ b -> Expr -> Fix (ExprFWith b)
  , exprWithout     -- ^             Fix (ExprFWith b) -> Expr
  , mapExprFWith    -- ^ (b -> c) -> Fix (ExprFWith b) -> Fix (ExprFWith c)

  , depth          -- ^ Expr -> Int
  , refExprVariety -- ^ RefExpr -> (ExprCtr, Arity)
  , refExprArity   -- ^ RefExpr -> Arity
  , maxAddr        -- ^ Rslt -> Either String Addr
  , nextAddr       -- ^ Rslt -> Either String Addr
  ) where

import           Data.Functor.Foldable
import           Data.Either hiding (lefts)
import           Data.Maybe
import qualified Data.Map       as M
import qualified Data.Set       as S

import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | = Rel

instance LeftStrings Rel where
  ifLefts (Rel es e) = prefixLeft "ifLefts_rel: " $
    let lefts = filter isLeft $ e : es
        fr = fromRight $ error "impossible"
        fl = fromLeft $ error "impossible"
    in case null lefts of
         True -> Right $ Rel (map fr es) (fr e)
         False -> Left $ concat $ map fl lefts

instance LeftStrings Tplt where
  ifLefts (Tplt fore mids aft) = prefixLeft "ifLefts_tplt: " $
    let as = maybeToList fore ++ mids ++ maybeToList aft
        lefts = filter isLeft as
        fr = fromRight $ error "impossible"
        fl = fromLeft $ error "impossible"
    in case null lefts of
         True -> Right $ Tplt  (fmap fr fore)  (map fr mids)  (fmap fr aft)
         False -> Left $ concat $ map fl lefts

replaceNth_tplt :: a -> Int -> Tplt a -> Either String (Tplt a)
replaceNth_tplt a' 0 (Tplt (Just _) bs c) =
  Right $ Tplt (Just a') bs c
replaceNth_tplt a' n (Tplt a bs c) =
  prefixLeft "replaceNth_tplt: " $
  if n <= length bs
  then do bs' <- replaceNth a' n bs
          Right $ Tplt a bs' c
  else if n == length bs + 1
       then ( if null c
              then Left $ "Optional last joint not present."
              else Right $ Tplt a bs $ Just a' )
       else Left $ "Index greater than size of tplt."

-- | PITFALL: Lossy.
tpltToList :: Tplt a -> [a]
tpltToList (Tplt a bs c) =
  maybeToList a ++ bs ++ maybeToList c


-- | = ExprFWith

toExprWith :: forall b. b -> Expr -> Fix (ExprFWith b)
toExprWith b x = Fix $ EFW (b, f x) where
  f :: Expr -> ExprF (Fix (ExprFWith b))
  f (Addr a)             = AddrF a
  f (Phrase p)           = PhraseF p
  f (ExprTplt js)        = ExprTpltF $
        fmap (toExprWith b) js
  f (ExprRel (Rel ms t)) = ExprRelF $
    Rel (map (toExprWith b) ms) (toExprWith b t)

exprWithout :: Fix (ExprFWith b) -> Expr
exprWithout (Fix (EFW (_, x))) = f x where
  f :: ExprF (Fix (ExprFWith b)) -> Expr
  f (AddrF a) = Addr a
  f (PhraseF p) = Phrase p
  f (ExprRelF (Rel ms t)) = ExprRel $
    Rel (map exprWithout ms) $ exprWithout t
  f (ExprTpltF js) = ExprTplt $
        fmap exprWithout js

mapExprFWith :: forall b c.
  (b -> c) -> Fix (ExprFWith b) -> Fix (ExprFWith c)
mapExprFWith f (Fix (EFW (b,x))) = Fix $ EFW (f b, g x) where
  g :: ExprF (Fix (ExprFWith b)) -> ExprF (Fix (ExprFWith c))
  g (AddrF a) = AddrF a
  g (PhraseF a) = PhraseF a
  g (ExprRelF (Rel ms t)) = ExprRelF $ Rel
    (map (mapExprFWith f) ms) $ mapExprFWith f t
  g (ExprTpltF js) = ExprTpltF $
    fmap (mapExprFWith f) js


-- | = For `Expr`s

depth :: Expr -> Int
depth = cata f where
  f :: Base Expr Int -> Int
  f (AddrF _)               = 0
  f (PhraseF _)             = 0
  f (ExprRelF (Rel mems _)) = 1 +
    (if null mems then 0 else maximum mems)
  f (ExprTpltF _)           = 0


-- | for `RefExpr`s

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Phrase'  _) = (PhraseCtr, 0)
refExprVariety e@(Tplt'  _)   = (TpltCtr, refExprArity e)
refExprVariety e@(Rel' _)     = (RelCtr , refExprArity e)

refExprArity :: RefExpr -> Arity
refExprArity (Phrase' _)           = 0
refExprArity (Rel' (Rel x _))      = length x
refExprArity (Tplt' (Tplt _ js _)) = length js + 1


-- | = for `Rslt`s

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _addrToRefExpr
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)
