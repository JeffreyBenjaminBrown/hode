-- | Create `HExpr`s from `PExpr`s and `PRel`s.
-- The step that follows parsing.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.Convert where

import qualified Data.Map       as M

import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes
import Util.Misc


-- | = Building an `HExpr` from a `PExpr`.

pRelToHExpr :: PRel -> Either String HExpr
pRelToHExpr Absent = Left "pRelToHExpr: cannot convert Absent."
pRelToHExpr (Open _ ms js) = pRelToHExpr $ Closed ms js

pRelToHExpr (Closed ms js) = do
  let t = Tplt $ map Word js
      ms' = filter f ms
        where f :: PRel -> Bool
              f Absent = False
              f (PNonRel px) = pExprIsSpecific px
              f _ = True
  (hms :: [HExpr]) <- ifLefts "pRelToHExpr"
    $ map pRelToHExpr ms'
  Right $ HMap
    $ M.insert RoleTplt (HExpr t)
    $ M.fromList $ zip (map RoleMember [1..]) hms

pRelToHExpr (PNonRel pn) = pExprToHExpr pn


pExprToHExpr :: PExpr -> Either String HExpr
pExprToHExpr px@(pExprIsSpecific -> False) = Left
  $ "pExprToHExpr: " ++ show px ++ " is not specific enough."

pExprToHExpr (PExpr s)       = Right $ HExpr s
pExprToHExpr (PMap m)        = HMap <$> pMapToHMap m
pExprToHExpr (PEval pnr)     = do
  (x :: HExpr)  <- pExprToHExpr pnr
  Right $ HEval x $ pathsToIts_pExpr pnr
pExprToHExpr (PVar s)        = Right $ HVar s
pExprToHExpr (PDiff a b)     = do a' <- pExprToHExpr a
                                  b' <- pExprToHExpr b
                                  return $ HDiff a' b'
pExprToHExpr (PAnd xs)       = do
  (l :: [HExpr]) <- ifLefts "pExprToHExpr" $ map pExprToHExpr xs
  return $ HAnd l
pExprToHExpr (POr xs)       = do
  (l :: [HExpr]) <- ifLefts "pExprToHExpr" $ map pExprToHExpr xs
  return $ HOr l
pExprToHExpr (It (Just pnr)) = pExprToHExpr pnr
pExprToHExpr (PRel pr)       = pRelToHExpr pr

-- These redundant checks (to keep GHCI from warning me) should come last.
pExprToHExpr Any =
  Left $ "pExprToHExpr: Any is not specific enough."
pExprToHExpr (It Nothing) = Left
  $ "pExprToHExpr: It (Nothing) is not specific enough."


pMapToHMap :: PMap -> Either String HMap
pMapToHMap = ifLefts_map "pMapToHMap"
  . M.map pExprToHExpr
  . M.filter pExprIsSpecific


-- | = Finding the `It`s for a `PEval` to evaluate.

pathsToIts_pRel :: PRel -> [[Role]]
pathsToIts_pRel Absent = []
pathsToIts_pRel (PNonRel pnr) = pathsToIts_pExpr pnr
pathsToIts_pRel (Closed ms _) = let
  f :: (Int,[[Role]]) -> [[Role]]
  f (i,ps) = map ((:) $ RoleMember i) ps
  in concatMap f $ zip [1..] $ map pathsToIts_pRel ms
pathsToIts_pRel (Open _ ms js) = pathsToIts_pRel $ Closed ms js

pathsToIts_pExpr :: PExpr -> [[Role]]
pathsToIts_pExpr (PExpr _)       = []
pathsToIts_pExpr (PMap m)        =
  concatMap (\(role, paths) -> map ((:) role) paths) $ M.toList
  $ M.map pathsToIts_pExpr m
pathsToIts_pExpr (PEval pnr)     = pathsToIts_pExpr pnr
pathsToIts_pExpr (PVar _)        = []
pathsToIts_pExpr x@(PDiff _ _)      =
  error $ "pathsToIts_pExpr: called on PDiff: " ++ show x
pathsToIts_pExpr x@(PAnd _)      =
  error $ "pathsToIts_pExpr: called on PAnd: " ++ show x
pathsToIts_pExpr x@(POr _)      =
  error $ "pathsToIts_pExpr: called on POr: " ++ show x
pathsToIts_pExpr Any             = []
pathsToIts_pExpr (It Nothing)    = [[]]
  -- the unique way to get to an It from here is to stay still
pathsToIts_pExpr (It (Just pnr)) = [] : pathsToIts_pExpr pnr
pathsToIts_pExpr x@(PPar _ _)      =
  error $ "pathsToIts_pExpr: called on PPar: " ++ show x
pathsToIts_pExpr (PRel pr)       = pathsToIts_pRel pr
