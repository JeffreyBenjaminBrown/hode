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

x = R._replaceInRefExpr D.rslt
    (RoleInTplt' $ RoleCap CapRight) 1
    (Tplt'$ Tplt Nothing [3] Nothing)

y = R._replaceInRefExpr D.rslt_rightCapped
    (RoleInTplt' $ RoleCap CapRight) 2
    (Tplt'$ Tplt Nothing [3] Nothing)
