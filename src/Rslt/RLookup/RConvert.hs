{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RLookup.RConvert where

import qualified Data.Map       as M

import Rslt.RTypes
import Util.Misc


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Phrase' w) = Right $ Phrase w
refExprToExpr r (Tplt' jointAs) = do
  (jointEs  :: [RefExpr])   <-
    ifLefts "refExprToExpr" $ map (addrToRefExpr r) jointAs
  (jointEis :: [Expr]) <-
    ifLefts "refExprToExpr" $ map (refExprToExpr r) jointEs
  Right $ ExprTplt jointEis

refExprToExpr r (Rel' (Rel memAs tA)) = do
  (memREs  :: [RefExpr]) <- ifLefts    "refExprToExpr"
                          $ map (addrToRefExpr r) memAs
  (memEs :: [Expr])    <- ifLefts    "refExprToExpr"
                           $ map (refExprToExpr r) memREs
  (tE     :: RefExpr)   <- prefixLeft "refExprToExpr"
                           $ addrToRefExpr r tA
  (tEi    :: Expr)      <- prefixLeft "refExprToExpr"
                           $ refExprToExpr r tE
  Right $ ExprRel $ Rel memEs tEi

refExprToExpr r (Par' sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr]) <- ifLefts "refExprToExpr" $ map (addrToRefExpr r) as
  (eis :: [Expr])    <- ifLefts "refExprToExpr" $ map (refExprToExpr r) es
  Right $ ExprPar (zip ss eis) s


-- | == Lookup from an `Expr`

exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr x img =
  let pel = prefixLeft "exprToAddr"
  in case img of
  Phrase w -> pel $ refExprToAddr x $ Phrase' w

  Addr a -> pel (addrToRefExpr x a) >>= const (Right a)

  ExprTplt is -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    pel $ refExprToAddr x $ Tplt' mas

  ExprRel (Rel is i) -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    ma <- pel $ exprToAddr x i
    pel $ refExprToAddr x (Rel' $ Rel mas ma)

  ExprPar _ _ -> Left $ "exprToAddr: Pars are not in index, "
    ++ "cannot be looked up.\n"


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
