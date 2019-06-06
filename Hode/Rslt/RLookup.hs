{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.RLookup (
  -- | = primary lookup functions
    variety -- ^ Rslt -> Addr -> Either String (ExprCtr, Arity)
  , arity   -- ^ Rslt -> Expr -> Either String Arity
  , has     -- ^ Rslt -> Addr -> Either String (Map Role Addr)
  , hasInRole -- ^ Rslt -> Role -> Addr -> Either String Addr
  , isIn    -- ^ Rslt -> Addr -> Either String (Set (Role,Addr))
  , fills   -- ^ Rslt -> (Role, Addr) -> Either String Addr

  -- | = convert bewteen `Addr`, `Expr`, `RefExpr`
  , C.refExprToExpr -- ^ Rslt -> RefExpr -> Either String Expr
  , C.exprToAddr    -- ^ Rslt -> Expr    -> Either String Addr
  , C.addrToRefExpr -- ^ Rslt -> Addr    -> Either String RefExpr
  , C.addrToExpr    -- ^ Rslt -> Addr    -> Either String Expr
  , C.refExprToAddr -- ^ Rslt -> RefExpr -> Either String Addr

  -- | = misc
  , findSubExprs -- ^ [RolePath] -> Either Addr Var -> Find Addr Rslt
  , subExprs     -- ^ Rslt -> [RolePath] -> Addr -> Either String (Set Addr)
  , subExpr      -- ^ Rslt -> Addr -> RolePath   -> Either String Addr
  , unAddr       -- ^ Rslt -> Expr               -> Either String Expr
  , unAddrRec    -- ^ Rslt -> Expr               -> Either String Expr
  ) where

import           Data.Functor (void)
import           Data.Functor.Foldable
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Qseq.MkLeaf
import Hode.Qseq.QTypes
import Hode.Rslt.RLookup.RConvert
import qualified Hode.Rslt.RLookup.RConvert as C
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | = primary lookup functions

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

arity :: Rslt -> Expr -> Either String Arity
arity r (Addr a)  = snd <$> variety r a
arity _ (Phrase _)           = Right 0
arity r (ExprRel (Rel ms t)) = prefixLeft "arity" $ do
  ta <- arity r t
  if ta == length ms then Right ta
    else Left $ "Rel Tplt " ++ show t
         ++ " does not match number of Rel members " ++ show ms ++ ".\n"
arity _ (ExprTplt x)         = Right $ length x - 1


-- | `has r a` finds the `RefExpr` `re` at `a` in `r`, and returns
-- every position contained in `re`.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = prefixLeft "-> has" $ do
  void $ addrToRefExpr r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | hasInRole r rl maybeMbr host` determines whether
-- `host` contains `maybeMbr` in the `rl` position.
hasInRole :: Rslt -> Role -> Addr -> Either String Addr
hasInRole r rl a = prefixLeft "hasInRole: " $ do
  x :: Maybe Addr <- M.lookup rl <$> has r a
  maybe (Left $ show a ++ "not found") Right x


-- | `isIn r a` finds the `RefExpr` `e` at `a` in `r`, and returns
-- every position that `re` occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a =
  prefixLeft "-> isIn" $ do
  void $ addrToRefExpr r a
  maybe (Right S.empty) Right $ M.lookup a $ _isIn r

-- | `fills r (role,a)` finds the `Addr` of the `Expr` that
-- occupies `role` in `a`.
fills :: Rslt -> (Role, Addr) -> Either String Addr
fills x (r,a) = prefixLeft "-> fills" $ do
  (positions :: Map Role Addr) <- has x a
  let err = Left $ "role " ++ show r
            ++ " not among positions in RefExpr at " ++ show a
  maybe err Right $ M.lookup r positions


-- | == build `Query`s for `Rslt`s

findSubExprs :: [RolePath] -> Either Addr Var -> Find Addr Rslt
findSubExprs paths = mkFindFrom f where
  f :: Rslt -> Addr -> Either String (Set Addr)
  f r a = subExprs r paths a


-- | = Find sub-`Expr`s of an `Expr`

subExprs :: Rslt -> [RolePath] -> Addr -> Either String (Set Addr)
subExprs r rls a =
  prefixLeft "-> subExprs" $ S.fromList <$> ifLefts its
  where its :: [Either String Addr]
        its = map (subExpr r a) rls

subExpr :: Rslt -> Addr -> RolePath -> Either String Addr
subExpr _ a [] = Right a
subExpr r a (rl : rls) = prefixLeft "-> subExpr" $ do
  (aHas :: Map Role Addr) <-
    prefixLeft (", looking up Addr" ++ show a)
    $ has r a
  (member_of_a :: Addr) <-
    maybe (Left $ "looking up Role " ++ show rl ++ ".") Right
    $ M.lookup rl aHas
  subExpr r member_of_a rls


-- | == A utility

unAddr :: Rslt -> Expr -> Either String Expr
unAddr r (Addr a) = addrToExpr r a
unAddr _ e        = Right e

unAddrRec :: Rslt -> Expr -> Either String Expr
unAddrRec r = cata f where
  f :: Base Expr (Either String Expr) -> Either String Expr
  f (AddrF a) = addrToExpr r a
  -- TODO ? AddrF is the only interesting case. Is there a
  -- good way to simplify the following boilerplate?
  f (PhraseF p) = Right $ Phrase p
  f (ExprRelF (Rel ms t)) = do ms' <- ifLefts ms
                               t' <- t
                               Right $ ExprRel $ Rel ms' t'
  f (ExprTpltF js) = do js' <- ifLefts js
                        Right $ ExprTplt js'
