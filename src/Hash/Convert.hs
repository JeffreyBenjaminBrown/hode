-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.Convert where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hash.HTypes
import Qseq.QTypes
import Rslt.RTypes
import Util.Misc


pRelToHExpr :: PRel -> Either String HExpr
pRelToHExpr Absent = Left "pRelToHExpr: cannot convert Absent."
pRelToHExpr (PNonRel pn) = pNonRelToHExpr pn
pRelToHExpr (Open _ ms js) = pRelToHExpr $ Closed ms js
pRelToHExpr (Closed ms js) = do
  let t = Tplt $ map Word js
  (hms :: [HExpr]) <- ifLefts "pRelToHExpr" $ map pRelToHExpr ms
  Right $ HMap
    $ M.insert RoleTplt (HExpr t)
    $ M.fromList $ zip (map RoleMember [1..]) hms

pNonRelToHExpr :: PNonRel -> Either String HExpr
pNonRelToHExpr (PWord s) = Right $ HExpr $ Word s
pNonRelToHExpr (PVar s) = Right $ HVar s
pNonRelToHExpr Any = Left $ "pNonRelToHExpr: Cannot convert Any."
