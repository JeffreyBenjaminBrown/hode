-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Hash.HUtil where

import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Set (Set)
import qualified Data.Set  as S

import           Hash.HTypes
import           Qseq.QTypes (Var)
import           Rslt.RTypes


-- | = for Hash

hVars :: HExpr -> Set Var
hVars (HMap m)    = S.unions $ map hVars $ M.elems m
hVars (HEval m _) = hVars m
hVars (HVar v)    = S.singleton v
hVars (HExpr _)   = S.empty
hVars (HDiff h i) = S.union (hVars h) (hVars i)
hVars (HAnd hs)   = S.unions $ map hVars hs
hVars (HOr hs)    = S.unions $ map hVars hs


-- | = for parsing Hash

pnrWord :: String -> PRel
pnrWord = PNonRel . PExpr . Word

pExprIsSpecific :: PExpr -> Bool
pExprIsSpecific (PMap m)       = or $ map pExprIsSpecific $ M.elems m
pExprIsSpecific (PEval px)     =          pExprIsSpecific px
pExprIsSpecific Any            = False
pExprIsSpecific (It Nothing)   = False
pExprIsSpecific (It (Just px)) =          pExprIsSpecific px
pExprIsSpecific _              = True


-- | To simplify a `PRel` or `PExpr` is to flatten sections of the form
-- `PNonRel (PRel x)` into `x`, and similarly sections of the form
-- `PRel (PNonRel x)`.
--
-- TODO ? It would be nice to use some kind of recursion scheme, so that
-- I defined once how to map over `PRel`s and `PExpr`s, allowing any
-- function so mapped to be much simpler.

simplifyPRel :: PRel -> PRel
-- These three cases are actual simplifications
simplifyPRel (PNonRel (PRel pr)) = simplifyPRel pr
simplifyPRel (Open _ [PNonRel pnr] []) = PNonRel $ simplifyPExpr pnr
simplifyPRel (Closed [PNonRel pnr] []) = PNonRel $ simplifyPExpr pnr
-- The rest are just mapping into contents.
simplifyPRel Absent = Absent
simplifyPRel (PNonRel pnr) = PNonRel $ simplifyPExpr pnr
simplifyPRel (Open l xs js) = Open l (map simplifyPRel xs) js
simplifyPRel (Closed xs js) = Closed (map simplifyPRel xs) js

simplifyPExpr :: PExpr -> PExpr
 -- These are the simplifications.
simplifyPExpr (PRel (PNonRel pnr)) = simplifyPExpr pnr
simplifyPExpr (PEval (PEval x)) = simplifyPExpr $ PEval x
simplifyPExpr (PDiff a b) = PDiff (simplifyPExpr a) (simplifyPExpr b)
simplifyPExpr (PAnd xs) = let
  xs' = map simplifyPExpr xs
  (ands,others) = L.partition (\case PAnd _ -> True; _ -> False) xs'
  in PAnd $ concatMap (\(PAnd c) -> c) ands ++ others
simplifyPExpr (POr xs) = let
  xs' = map simplifyPExpr xs
  (ors,others) = L.partition (\case POr _ -> True; _ -> False) xs'
  in POr $ concatMap (\(POr c) -> c) ors ++ others

-- The rest just map simplification into contents.
simplifyPExpr x@(PExpr _)     = x
simplifyPExpr (PMap m)        = PMap $ M.map simplifyPExpr m
simplifyPExpr (PEval x)       = PEval $ simplifyPExpr x
simplifyPExpr x@(PVar _)      = x
simplifyPExpr x@Any           = x
simplifyPExpr x@(It Nothing)  = x
simplifyPExpr   (It (Just x)) = It $ Just $ simplifyPExpr x
simplifyPExpr (PPar pairs s) = let
  (ss,xs) = unzip pairs
  xs' = map simplifyPExpr xs
  in PPar (zip ss xs') s
simplifyPExpr (PRel pr) = PRel $ simplifyPRel pr
