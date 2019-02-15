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
pNonRelToHExpr (PExpr s)       = Right $ HExpr s
pNonRelToHExpr (PVar s)        = Right $ HVar s
pNonRelToHExpr Any             = Left $ "pNonRelToHExpr: Cannot convert Any."
pNonRelToHExpr (It Nothing)    = Left $ "pNonRelToHExpr: Cannot convert empty It."
pNonRelToHExpr (It (Just pnr)) = pNonRelToHExpr pnr
pNonRelToHExpr (Eval pnr)      = error "todo: pNonRelToHExpr (Eval pnr)"
pNonRelToHExpr (PRel pr)       = pRelToHExpr pr

pathsToIts_pRel :: PRel -> [[Role]]
pathsToIts_pRel Absent = []
pathsToIts_pRel (PNonRel pnr) = pathsToIts_pNonRel pnr
pathsToIts_pRel (Closed ms _) = let
  f :: (Int,[[Role]]) -> [[Role]]
  f (i,ps) = map ((:) $ RoleMember i) ps
  in concatMap f $ zip [1..] $ map pathsToIts_pRel ms
pathsToIts_pRel (Open _ ms js) = pathsToIts_pRel $ Closed ms js

pathsToIts_pNonRel :: PNonRel -> [[Role]]
pathsToIts_pNonRel (PRel pr) = pathsToIts_pRel pr
pathsToIts_pNonRel (Eval pnr) = pathsToIts_pNonRel pnr
pathsToIts_pNonRel (It Nothing) = [[]]
  -- the unique way to get to an It from here is to stay still
pathsToIts_pNonRel (It (Just pnr)) = [] : pathsToIts_pNonRel pnr
pathsToIts_pNonRel (PMap m) =
  concatMap (\(role, paths) -> map ((:) role) paths)
  $ M.toList $ M.map pathsToIts_pNonRel m
pathsToIts_pNonRel (PExpr _) = []
pathsToIts_pNonRel (PVar _)  = []
pathsToIts_pNonRel Any       = []
