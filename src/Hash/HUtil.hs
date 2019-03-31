-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Hash.HUtil (
    hVars           -- ^ HExpr  -> Set Var
  , pnrPhrase       -- ^ String -> PRel
  , pExprIsSpecific -- ^ PExpr  -> Bool
  , pExprIsUnique   -- ^ PExpr  -> Bool
  , simplifyPRel    -- ^ PRel   -> PRel
  , simplifyPExpr   -- ^ PExpr  -> PExpr
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Set (Set)
import qualified Data.Set  as S

import           Hash.HTypes
import           Qseq.QTypes (Var)
import           Rslt.RTypes


-- | = for Hash

hVars :: HExpr -> Set Var
hVars = cata f where
  f :: Base HExpr (Set Var) -> Set Var
  f (HMapF m)    = S.unions $ M.elems m
  f (HEvalF m _) = m
  f (HVarF v)    = S.singleton v
  f (HExprF _)   = S.empty
  f (HDiffF h i) = S.union h i
  f (HAndF hs)   = S.unions hs
  f (HOrF hs)    = S.unions hs


-- | = for parsing Hash

pnrPhrase :: String -> PRel
pnrPhrase = PNonRel . PExpr . Phrase

pExprIsSpecific :: PExpr -> Bool
pExprIsSpecific = cata f where
  f :: Base PExpr Bool -> Bool
  f (PMapF m)       = or $ M.elems m
  f (PEvalF px)     = px
  f AnyF            = False
  f (ItF Nothing)   = False
  f (ItF (Just px)) = px
  f _               = True

pExprIsUnique :: PExpr -> Bool
pExprIsUnique = \case PExpr _ -> True
                      _       -> False

-- | To simplify a `PRel` or `PExpr` is to flatten sections of the form
-- `PNonRel (PRel x)` into `x`, and similarly sections of the form
-- `PRel (PNonRel x)`.

simplifyPRel :: PRel -> PRel
simplifyPRel = cata f where
  -- TODO ? Wart: Any `PNonRelF` pattern (here, 2) require recursing by hand.
  -- If I unified PRel and PExpr as an indexed type family,
  -- this clause could be eliminated. c.f. https://www.reddit.com/r/haskell/comments/3sm1j1/how_to_mix_the_base_functorrecursion_scheme_stuff/
  f :: Base PRel PRel -> PRel
  f (PNonRelF (PRel x)) =           simplifyPRel  x
  f (PNonRelF x)        = PNonRel $ simplifyPExpr x
  f (OpenF _ [PNonRel x] []) = PNonRel x
  f (ClosedF [PNonRel x] []) = PNonRel x
  f x                        = embed x

simplifyPExpr :: PExpr -> PExpr
simplifyPExpr = cata f where
  f :: Base PExpr PExpr -> PExpr
  -- TODO ? Wart: Any `PRelF` pattern (here, 2) require recursing by hand.
  -- See corresponding comment for `simplifyPRel`.
  f (PRelF (PNonRel x)) =        simplifyPExpr x
  f (PRelF x)           = PRel $ simplifyPRel  x
  f (PEvalF (PEval x))  = PEval x
  f (PAndF xs) = PAnd $ concatMap (\(PAnd c) -> c) ands ++ others where
    (ands, others) = L.partition (\case PAnd _ -> True; _ -> False) xs
  f (POrF xs)  = POr  $ concatMap (\(POr  c) -> c) ors  ++ others where
    (ors,  others) = L.partition (\case POr  _ -> True; _ -> False) xs
  f x                   = embed x
