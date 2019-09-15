-- Currently, this is entirely exported from Hode.Rslt.RLookup.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.RLookup.RConvert where

import qualified Data.Map       as M

import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Phrase' w) = Right $ Phrase w
refExprToExpr r (Tplt' (tas :: Tplt Addr)) =
  prefixLeft "-> refExprToExpr: " $ do
    tfs :: Tplt RefExpr <- ifLefts_tplt $ fmap (addrToRefExpr r) tas
    tes :: Tplt Expr    <- ifLefts_tplt $ fmap (refExprToExpr r) tfs
    Right $ ExprTplt tes

refExprToExpr r (Rel' (ras :: Rel Addr)) =
  prefixLeft "-> refExprToExpr: " $ do
    rfs :: Rel RefExpr <- ifLefts_rel $ fmap (addrToRefExpr r) ras
    res :: Rel Expr    <- ifLefts_rel $ fmap (refExprToExpr r) rfs
    Right $ ExprRel res


-- | == Lookup from an `Expr`

-- | `exprToAddr r e` converts every sub-`Expr` of `e` into a `RefExpr`,
-- and then uses `refExprToAddr`.
exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr r e = prefixLeft "exprToAddr: " $
  case e of
    Phrase w -> refExprToAddr r $ Phrase' w
  
    Addr a -> addrToRefExpr r a >>= const (Right a)
  
    ExprTplt te -> do
      tr <- ifLefts_tplt $ fmap (exprToAddr r) te
      refExprToAddr r $ Tplt' tr

    ExprRel re -> do
      rr <- ifLefts_rel $ fmap (exprToAddr r) re
      refExprToAddr r $ Rel' rr


-- | == Lookup from `Addr`s or `RefExpr`s. (These are convenience
-- functions for Map.exprToAddr applied to an Rslt field.)

addrToRefExpr :: Rslt -> Addr -> Either String RefExpr
addrToRefExpr r a =
  maybe (Left $ "addrToRefExpr: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _addrToRefExpr r

addrToExpr :: Rslt -> Addr -> Either String Expr
addrToExpr r a = addrToRefExpr r a >>= refExprToExpr r

refExprToAddr :: Rslt -> RefExpr -> Either String Addr
refExprToAddr r e = maybe err Right $
                    M.lookup e $ _refExprToAddr r
  where err = Left $ "refExprToAddr: RefExpr " ++
              show e ++ " not found.\n"
