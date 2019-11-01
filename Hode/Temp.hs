{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Lens.Micro hiding (has)
import           Test.HUnit

import           Hode.Rslt.RLookup hiding (exprToAddr)
import qualified Hode.Rslt.Edit         as R
import qualified Hode.Rslt.Edit.Initial as R
import qualified Hode.Rslt.Edit.Replace as R
import           Hode.Rslt.Index
import           Hode.Rslt.RTypes
import           Hode.Rslt.RValid
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D
import           Hode.Util.Misc


-- R.replaceInRole (RoleInRel' $ RoleMember 2) 1 5 D.rslt



newRel      :: Rel Addr = Rel [1,3] 4
new_refExpr :: RefExpr  = Rel' newRel
new_expr    :: Expr     = ExprRel $ fmap Addr newRel
refExprs    :: M.Map Addr RefExpr =
  D.refExprs & M.insert 5 new_refExpr

t1 = R.replaceExpr 5 new_expr D.rslt
t2 = Right (mkRslt refExprs)

