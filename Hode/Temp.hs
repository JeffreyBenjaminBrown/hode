{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import qualified Data.Map as M
import qualified Data.Set as S

import Hode.Rslt.RTypes
import Hode.Hash.HTypes


h = HMap $ M.fromList
    [ ( RoleTplt,
        HExpr $ ExprTplt $ Tplt
        (Just (Phrase "")) [Phrase ""] Nothing ),
      ( RoleMember 3,
        HExpr $ Phrase "b") ]


