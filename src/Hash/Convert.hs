-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE LambdaCase #-}
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
pRelToHExpr (Open _ ms js) = pRelToHExpr $ Closed ms js
pRelToHExpr (Closed ms js) = do
  let t = Tplt $ map Word js
  (hms :: [HExpr]) <- ifLefts "pRelToHExpr" $ map pRelToHExpr ms
  Right $ HMap
    $ M.insert RoleTplt (HExpr t)
    $ M.fromList $ zip (map RoleMember [1..]) hms
pRelToHExpr (PNonRel pn) = pExprToHExpr pn

pExprToHExpr :: PExpr -> Either String HExpr
pExprToHExpr (PExpr s)       = Right $ HExpr s
pExprToHExpr (PMap m)        = HMap <$> pMapToHMap m
pExprToHExpr (PEval pnr)     = do
  (x :: HExpr)  <- pExprToHExpr pnr
  Right $ HEval x $ pathsToIts_pExpr pnr
pExprToHExpr (PVar s)        = Right $ HVar s
pExprToHExpr Any             = Left $ "pExprToHExpr: Cannot convert Any."
pExprToHExpr (It Nothing)    = Left $ "pExprToHExpr: Cannot convert empty It."
pExprToHExpr (It (Just pnr)) = pExprToHExpr pnr
pExprToHExpr (PRel pr)       = pRelToHExpr pr

pMapToHMap :: PMap -> Either String HMap
pMapToHMap = ifLefts_map "pMapToHMap"
  . M.map pExprToHExpr
  . M.filter (\case It Nothing -> False; Any -> False; _ -> True)

pathsToIts_pRel :: PRel -> [[Role]]
pathsToIts_pRel Absent = []
pathsToIts_pRel (PNonRel pnr) = pathsToIts_pExpr pnr
pathsToIts_pRel (Closed ms _) = let
  f :: (Int,[[Role]]) -> [[Role]]
  f (i,ps) = map ((:) $ RoleMember i) ps
  in concatMap f $ zip [1..] $ map pathsToIts_pRel ms
pathsToIts_pRel (Open _ ms js) = pathsToIts_pRel $ Closed ms js

pathsToIts_pExpr :: PExpr -> [[Role]]
pathsToIts_pExpr (PExpr _) = []
pathsToIts_pExpr (PMap m) =
  concatMap (\(role, paths) -> map ((:) role) paths) $ M.toList
  $ M.map pathsToIts_pExpr m
pathsToIts_pExpr (PEval pnr) = pathsToIts_pExpr pnr
pathsToIts_pExpr (PVar _)  = []
pathsToIts_pExpr Any       = []
pathsToIts_pExpr (It Nothing) = [[]]
  -- the unique way to get to an It from here is to stay still
pathsToIts_pExpr (It (Just pnr)) = [] : pathsToIts_pExpr pnr
pathsToIts_pExpr (PRel pr) = pathsToIts_pRel pr
