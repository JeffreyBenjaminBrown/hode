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


-- | = Search

lookup :: Rslt -> ImgOfExpr -> Either String Addr
lookup x img =
  let pel = prefixLeft "lookup"
  in case img of
  ImgOfExpr e -> pel $ addrOf x e

  ImgOfAddr a -> pel (exprAt x a) >>= const (Right a)

  ImgOfTplt is -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    pel $ addrOf x $ Tplt mas

  ImgOfRel is i -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    ma <- pel $ lookup x i
    pel $ addrOf x (Rel mas ma)

exprAt :: Rslt -> Addr -> Either String Expr
exprAt r a =
  maybe (Left $ "addrOf: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _exprAt r

addrOf :: Rslt -> Expr -> Either String Addr
addrOf r e = maybe err Right $ M.lookup e $ _addrOf r
  where err = Left $ "addrOf: Expr " ++ show e ++ " not found.\n"

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = do
  either (\s -> Left $ "has: " ++ s) Right $ exprAt r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a = do
  either (\s -> Left $ "isIn: " ++ s) Right $ exprAt r a
  maybe (Right S.empty) Right $ M.lookup a $ _isIn r

-- | `fills r (role,a)` finds the expression that occupies
-- role in a.
fills :: Rslt -> (Role, Addr) -> Either String Addr
fills x (r,a) = do
  (positions :: Map Role Addr) <-
    prefixLeft "fills" $ has x a
  let err = Left $ "fills: role " ++ show r
            ++ " not among positions in Expr at " ++ show a
  maybe err Right $ M.lookup r positions
