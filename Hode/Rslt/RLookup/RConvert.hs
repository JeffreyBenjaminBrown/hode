-- Currently, this is entirely exported from Hode.Rslt.RLookup.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.RLookup.RConvert (
    refExprToExpr -- ^ Rslt -> RefExpr -> Either String Expr
  , addrToExpr    -- ^ Rslt -> Addr    -> Either String Expr
  , exprToAddr    -- ^ Rslt -> Expr    -> Either String Addr
  , addrToRefExpr -- ^ Rslt -> Addr    -> Either String RefExpr
  , refExprToAddr -- ^ Rslt -> RefExpr -> Either String Addr
  ) where

import qualified Data.Map       as M

import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Phrase' w) = Right $ Phrase w
refExprToExpr r (Tplt' (tas :: Tplt Addr)) =
  prefixLeft "refExprToExpr:" $ do
    tfs :: Tplt RefExpr <- ifLefts $ fmap (addrToRefExpr r) tas
    tes :: Tplt Expr    <- ifLefts $ fmap (refExprToExpr r) tfs
    Right $ ExprTplt tes

refExprToExpr r (Rel' (ras :: Rel Addr)) =
  prefixLeft "refExprToExpr:" $ do
    rfs :: Rel RefExpr <- ifLefts $ fmap (addrToRefExpr r) ras
    res :: Rel Expr    <- ifLefts $ fmap (refExprToExpr r) rfs
    Right $ ExprRel res


addrToExpr :: Rslt -> Addr -> Either String Expr
addrToExpr r a = addrToRefExpr r a >>= refExprToExpr r


-- | `exprToAddr r e` converts every sub-`Expr` of `e` into a `RefExpr`,
-- and then uses `refExprToAddr`.
exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr r e = prefixLeft "exprToAddr:" $
  case e of
    Phrase w -> refExprToAddr r $ Phrase' w
  
    ExprAddr a -> addrToRefExpr r a >>= const (Right a)
  
    ExprTplt te -> do
      tr <- ifLefts $ fmap (exprToAddr r) te
      refExprToAddr r $ Tplt' tr

    ExprRel re -> do
      rr <- ifLefts $ fmap (exprToAddr r) re
      refExprToAddr r $ Rel' rr


addrToRefExpr :: Rslt -> Addr -> Either String RefExpr
addrToRefExpr r a =
  maybe (Left $ "addrToRefExpr: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _addrToRefExpr r


refExprToAddr :: Rslt -> RefExpr -> Either String Addr
refExprToAddr r e = maybe err Right $
                    M.lookup e $ _refExprToAddr r
  where err = Left $ "refExprToAddr: RefExpr " ++
              show e ++ " not found.\n"
