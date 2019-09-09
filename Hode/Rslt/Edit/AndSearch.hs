-- PITFALL: These functions are all mutually recursive.
-- They mix editing and search.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Edit.AndSearch (
    exprToAddrInsert      -- ^ Rslt -> Expr   -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr] -> Either String (Rslt, [Addr])
  ) where

import qualified Data.List      as L

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc
import Hode.Rslt.Edit.Initial


-- | = Edit + search

-- | `exprToAddrInsert r ei` returns the `Addr` containing `ei`, if present.
-- If not, it inserts `ei`, and then returns the `Addr` containing it.
-- Since it might modify the `Rslt`, it also returns that.
exprToAddrInsert :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert r ei = do
  let (mra :: Maybe Addr) = either (const Nothing) Just
                            $ exprToAddr r ei
  case mra of
    Just a -> Right (r, a)
    Nothing -> exprToAddrInsert_rootNotFound r ei


-- | `exprToAddrInsert_rootNotFound` is like `exprToAddrInsert`, in the case
-- that the root `RefExpr` has been determined not to be present,
-- but the others still might be.
exprToAddrInsert_rootNotFound :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert_rootNotFound _ (Addr a) =
  Left $ "exprToAddrInsert: Addr " ++ show a ++ "not found.\n"

exprToAddrInsert_rootNotFound r0 (Phrase w) = do
  a <- nextAddr r0
  r1 <- insertAt a (Phrase' w) r0
  Right (r1,a)

exprToAddrInsert_rootNotFound r0 (ExprTplt js) = do
  (r1,as) <- prefixLeft "exprToAddrInsert_rootNotFound"
            $ exprToAddrInsert_list r0 js
  a <- nextAddr r1
  r2 <- insertAt a (Tplt' $ as) r1
  Right (r2, a)

exprToAddrInsert_rootNotFound r0 (ExprRel (Rel ms t)) =
  prefixLeft "exprToAddrInsert_rootNotFound" $ do
  (r1,ta)  <- exprToAddrInsert r0 t
  (r2,mas) <-  exprToAddrInsert_list r1 ms
  a <- nextAddr r2
  r3 <- insertAt a (Rel' $ Rel mas ta) r2
  Right (r3,a)

exprToAddrInsert_list :: Rslt -> [Expr] -> Either String (Rslt, [Addr])
exprToAddrInsert_list r0 is = do
  let ((er, as) :: (Either String Rslt, [[Addr]])) =
        L.mapAccumL f (Right r0) is where
          f :: Either String Rslt -> Expr -> (Either String Rslt, [Addr])
          f (Left s) _ = (Left s, error "irrelevant")
          f (Right r) ei = case exprToAddrInsert r ei of
                             Left s -> (Left s, error "irrelevant")
                             Right (r',a) -> (Right r', [a])
  r1 <- prefixLeft "exprToAddrInsert_list" er
  Right $ (r1, concat as)
