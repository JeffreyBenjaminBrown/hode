{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RLookup.RConvert where

import qualified Data.Map       as M

import Rslt.RTypes
import Util.Misc


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Phrase' w) = Right $ Phrase w
refExprToExpr r (Tplt' jointAs) =
  prefixLeft "-> refExprToExpr: " $ do
    (jointEs  :: [RefExpr])   <-
      ifLefts $ map (addrToRefExpr r) jointAs
    (jointEis :: [Expr]) <-
      ifLefts $ map (refExprToExpr r) jointEs
    Right $ ExprTplt jointEis

refExprToExpr r (Rel' (Rel memAs tA)) =
  prefixLeft "-> refExprToExpr: " $ do
    (memREs  :: [RefExpr]) <- ifLefts $ map (addrToRefExpr r) memAs
    (memEs :: [Expr])      <- ifLefts $ map (refExprToExpr r) memREs
    (tE     :: RefExpr)    <- addrToRefExpr r tA
    (tEi    :: Expr)       <- refExprToExpr r tE
    Right $ ExprRel $ Rel memEs tEi


-- | == Lookup from an `Expr`

exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr x img = prefixLeft "-> exprToAddr: " $ case img of
  Phrase w -> refExprToAddr x $ Phrase' w

  Addr a -> addrToRefExpr x a >>= const (Right a)

  ExprTplt is -> do
    mas <- ifLefts $ map (exprToAddr x) is
    refExprToAddr x $ Tplt' mas

  ExprRel (Rel is i) -> do
    mas <- ifLefts $ map (exprToAddr x) is
    ma <- exprToAddr x i
    refExprToAddr x (Rel' $ Rel mas ma)


-- | == Lookup from `Addr`s or `RefExpr`s. (These are convenience
-- functions for Map.exprToAddr applied to an Rslt field.)

addrToRefExpr :: Rslt -> Addr -> Either String RefExpr
addrToRefExpr r a =
  maybe (Left $ "addrToRefExpr: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _addrToRefExpr r

addrToExpr :: Rslt -> Addr -> Either String Expr
addrToExpr r a = addrToRefExpr r a >>= refExprToExpr r

refExprToAddr :: Rslt -> RefExpr -> Either String Addr
refExprToAddr r e = maybe err Right $ M.lookup e $ _refExprToAddr r
  where err = Left $ "refExprToAddr: RefExpr " ++ show e ++ " not found.\n"
