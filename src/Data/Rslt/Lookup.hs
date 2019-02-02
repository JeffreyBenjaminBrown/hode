{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Lookup where

import           Prelude hiding (lookup)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.Index
import Data.Rslt.RTypes
import Util


-- | = Lookup from `Expr`s

lookup :: Rslt -> Expr -> Either String Addr
lookup x img =
  let pel = prefixLeft "lookup"
  in case img of
  Word w -> pel $ addrOf x $ Word' w

  ExprAddr a -> pel (refExprAt x a) >>= const (Right a)

  Tplt is -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    pel $ addrOf x $ Tplt' mas

  Rel is i -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    ma <- pel $ lookup x i
    pel $ addrOf x (Rel' mas ma)

  Par _ _ -> Left $ "lookup: Pars are not in index, "
    ++ "cannot be looked up.\n"


-- | = Lookup from `Addr`s or `RefExpr`s

refExprAt :: Rslt -> Addr -> Either String RefExpr
refExprAt r a =
  maybe (Left $ "addrOf: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _refExprAt r

addrOf :: Rslt -> RefExpr -> Either String Addr
addrOf r e = maybe err Right $ M.lookup e $ _addrOf r
  where err = Left $ "addrOf: RefExpr " ++ show e ++ " not found.\n"

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = do
  either (\s -> Left $ "has: " ++ s) Right $ refExprAt r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a = do
  either (\s -> Left $ "isIn: " ++ s) Right $ refExprAt r a
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
