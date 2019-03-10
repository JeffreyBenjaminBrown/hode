{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RLookup where

import           Data.Functor (void)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes
import Qseq.MkLeaf
import Rslt.RTypes
import Util.Misc


-- | == build `Query`s for `Rslt`s

findSubExprs :: [[Role]] -> Either Addr Var -> Find Addr Rslt
findSubExprs paths = mkFindFrom f where
  f :: Rslt -> Addr -> Either String (Set Addr)
  f r a = subExprs r paths a


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Phrase' w) = Right $ Phrase w
refExprToExpr r (Tplt' jointAs) = do
  (jointEs  :: [RefExpr])   <-
    ifLefts "refExprToExpr" $ map (addrToRefExpr r) jointAs
  (jointEis :: [Expr]) <-
    ifLefts "refExprToExpr" $ map (refExprToExpr r) jointEs
  Right $ Tplt jointEis

refExprToExpr r (Rel' memAs tA) = do
  (memEs  :: [RefExpr]) <- ifLefts    "refExprToExpr"
                          $ map (addrToRefExpr r) memAs
  (memEis :: [Expr])    <- ifLefts    "refExprToExpr"
                           $ map (refExprToExpr r) memEs
  (tE     :: RefExpr)   <- prefixLeft "refExprToExpr"
                           $ addrToRefExpr r tA
  (tEi    :: Expr)      <- prefixLeft "refExprToExpr"
                           $ refExprToExpr r tE
  Right $ Rel memEis tEi

refExprToExpr r (Par' sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr]) <- ifLefts "refExprToExpr" $ map (addrToRefExpr r) as
  (eis :: [Expr])    <- ifLefts "refExprToExpr" $ map (refExprToExpr r) es
  Right $ Par (zip ss eis) s


-- | = Find sub-`Expr`s of an `Expr`

subExprs :: Rslt -> [[Role]] -> Addr -> Either String (Set Addr)
subExprs r rls a =
  S.fromList <$> ifLefts "subExprs" its
  where its :: [Either String Addr]
        its = map (subExpr r a) rls

subExpr :: Rslt -> Addr -> [Role] -> Either String Addr
subExpr _ a [] = Right a
subExpr r a (rl : rls) = do
  (aHas :: Map Role Addr) <-
    prefixLeft ("subExpr, looking up Addr" ++ show a)
    $ has r a
  (member_of_a :: Addr) <-
    maybe (Left $ "subExpr, looking up Role " ++ show rl ++ ".") Right
    $ M.lookup rl aHas
  subExpr r member_of_a rls


-- | == Lookup from an `Expr`

exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr x img =
  let pel = prefixLeft "exprToAddr"
  in case img of
  Phrase w -> pel $ refExprToAddr x $ Phrase' w

  Addr a -> pel (addrToRefExpr x a) >>= const (Right a)

  Tplt is -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    pel $ refExprToAddr x $ Tplt' mas

  Rel is i -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    ma <- pel $ exprToAddr x i
    pel $ refExprToAddr x (Rel' mas ma)

  Par _ _ -> Left $ "exprToAddr: Pars are not in index, "
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

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

arity :: Rslt -> Expr -> Either String Arity
arity r (Addr a)  = snd <$> variety r a
arity _ (Phrase _)  = Right 0
arity r (Rel ms t) = do
  ta <- arity r t
  if ta == length ms then Right ta
    else Left $ "arity: Rel Tplt " ++ show t
         ++ " does not match number of Rel members " ++ show ms ++ ".\n"
arity _ (Tplt x)  = Right $ length x - 1
arity _ (Par x _) = Right $ length x


-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = do
  void $ either (\s -> Left $ "has: " ++ s) Right $ addrToRefExpr r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a = do
  void $ either (\s -> Left $ "isIn: " ++ s) Right $ addrToRefExpr r a
  maybe (Right S.empty) Right $ M.lookup a $ _isIn r

-- | `fills r (role,a)` finds the expression that occupies
-- role in a.
fills :: Rslt -> (Role, Addr) -> Either String Addr
fills x (r,a) = do
  (positions :: Map Role Addr) <-
    prefixLeft "fills" $ has x a
  let err = Left $ "fills: role " ++ show r
            ++ " not among positions in RefExpr at " ++ show a
  maybe err Right $ M.lookup r positions


-- | = Misc

unAddr :: Rslt -> Expr -> Either String Expr
unAddr r (Addr a) = addrToExpr r a
unAddr _ e        = Right e
