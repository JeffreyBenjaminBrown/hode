{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import qualified Data.Map as M
--import qualified Data.Set as S

import Hode.Rslt.RTypes
import Hode.Hash.HTypes


p :: PExpr
p = PRel ( Open 1
           [ Absent,
             PNonRel (PExpr (Phrase "a")),
             PNonRel (PExpr (Phrase "b"))]
           ["",""] )

h :: HExpr
h = HMap $ M.fromList
  [ ( RoleTplt, ( HExpr $ ExprTplt $ Tplt
                  (Just $ Phrase "") [Phrase ""] Nothing) ),
   (RoleMember 2, HExpr $ Phrase "a"),
   (RoleMember 3, HExpr $ Phrase "b") ]
