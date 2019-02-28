-- | After parsing, the next step is to
-- create `HExpr`s from `PExpr`s and `PRel`s.

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

-- | In a PRel, outer *members* can be absent:
--
--   > parse pExpr "" "/hash x #j"
--   Right ( PRel ( Open 1 [ PNonRel $ PExpr $ Phrase "x"
--                         , Absent ]
--                  ["j"] ) )
--
-- In an HExpr, nothing is absent, but joints can be empty.
-- For every outer member of a PRel that is not Absent,
-- there should be an empty string added to that side of the
-- template in the corresponding HExpr.

pRelToHExpr :: PRel -> Either String HExpr
pRelToHExpr Absent = Left "pRelToHExpr: cannot convert Absent."
pRelToHExpr (Open _ ms js) = pRelToHExpr $ Closed ms js

pRelToHExpr (Closed ms js0) = do
  let absentLeft, absentRight :: Bool
      absentLeft  = case head ms of Absent -> True; _ -> False
      absentRight = case last ms of Absent -> True; _ -> False
      js1 = if not absentLeft  then "" : js0    else js0
      js2 = if not absentRight then js1 ++ [""] else js1
      t = Tplt $ map Phrase js2
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

pathsToIts_pExpr :: PExpr -> [[Role]]
pathsToIts_pExpr (PEval pnr) = pathsToIts_sub_pExpr pnr
pathsToIts_pExpr x           = pathsToIts_sub_pExpr x

pathsToIts_sub_pExpr :: PExpr -> [[Role]]
pathsToIts_sub_pExpr (PExpr _)       = []
pathsToIts_sub_pExpr (PMap m)        =
  concatMap (\(role, paths) -> map ((:) role) paths) $ M.toList
  $ M.map pathsToIts_sub_pExpr m
pathsToIts_sub_pExpr (PEval _)     = []
  -- don't recurse into a new PEval context; the paths to
  -- that PEval's `it`s are not the path to this one's.

pathsToIts_sub_pExpr (PVar _)        = []
pathsToIts_sub_pExpr x@(PDiff _ _)      =
  error $ "pathsToIts_sub_pExpr: called on PDiff: " ++ show x
pathsToIts_sub_pExpr x@(PAnd _)      =
  error $ "pathsToIts_sub_pExpr: called on PAnd: " ++ show x
pathsToIts_sub_pExpr x@(POr _)      =
  error $ "pathsToIts_sub_pExpr: called on POr: " ++ show x
pathsToIts_sub_pExpr Any             = []
pathsToIts_sub_pExpr (It Nothing)    = [[]]
  -- the unique way to get to an It from here is to stay still
pathsToIts_sub_pExpr (It (Just pnr)) = [] : pathsToIts_sub_pExpr pnr
pathsToIts_sub_pExpr x@(PPar _ _)      =
  error $ "pathsToIts_sub_pExpr: called on PPar: " ++ show x
pathsToIts_sub_pExpr (PRel pr)       = pathsToIts_sub_pRel pr

pathsToIts_sub_pRel :: PRel -> [[Role]]
pathsToIts_sub_pRel Absent = []
pathsToIts_sub_pRel (PNonRel pnr) = pathsToIts_sub_pExpr pnr
pathsToIts_sub_pRel (Closed ms _) = let
  f :: (Int,[[Role]]) -> [[Role]]
  f (i,ps) = map ((:) $ RoleMember i) ps
  in concatMap f $ zip [1..] $ map pathsToIts_sub_pRel ms
pathsToIts_sub_pRel (Open _ ms js) = pathsToIts_sub_pRel $ Closed ms js
