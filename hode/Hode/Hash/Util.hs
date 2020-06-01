-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE LambdaCase
, ScopedTypeVariables
#-}

module Hode.Hash.Util (
    hor             -- ^ [Addr] -> HExpr
  , hVars           -- ^ HExpr  -> Set Var
  , hSub            -- ^ M.Map Var HExpr -> HExpr -> HExpr
  , pnrPhrase       -- ^ String -> PRel
  , pExprIsSpecific -- ^ PExpr  -> Bool
  , pExprIsUnique   -- ^ PExpr  -> Bool
  , simplifyHExpr   -- ^ HExpr -> HExpr
  , simplifyPRel    -- ^ PRel   -> PRel
  , simplifyPExpr   -- ^ PExpr  -> PExpr
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Set (Set)
import qualified Data.Set  as S

import           Hode.Hash.Types
import           Hode.Qseq.Types (Var)
import           Hode.Rslt.Types


-- | = for Hash

hor :: [Addr] -> HExpr
hor = HOr . map (HExpr . ExprAddr)

hVars :: HExpr -> Set Var
hVars = cata f where
  f :: Base HExpr (Set Var) -> Set Var
  f (HMapF m)              = S.unions $ M.elems m
  f (HMemberHostsF m)      = m
  f (HMemberHostsRecF _ m) = m
  f (HEvalF m _)           = m
  f (HVarF v)              = S.singleton v
  f (HExprF _)             = S.empty
  f (HDiffF h i)           = S.union h i
  f (HAndF hs)             = S.unions hs
  f (HOrF hs)              = S.unions hs
  f (HReachF _ t s)        = S.unions $ [t,s]
  f (HTransF _ _ t e s)    = S.unions $ [t,e,s]
  f HTpltsF                = S.empty

-- | Substitute something(s) for the variable(s) in an `HExpr`.
hSub :: M.Map Var HExpr -> HExpr -> HExpr
hSub m = cata f where
  f :: Base HExpr HExpr -> HExpr
  f (HVarF v) = maybe (HVar v) id $ M.lookup v m
  f bh        = embed bh


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

-- | If an `HAnd` or `HOr` constructor contains a single item,
-- the expression can be reduced:
-- omitting the constructor introduces no semantic change.
simplifyHExpr :: HExpr -> HExpr
simplifyHExpr = cata f where
  f :: Base HExpr HExpr -> HExpr
  f = \case
    HAndF [x] -> x
    HOrF  [x] -> x
    wrapper   -> embed wrapper

-- | To simplify a `PRel` or `PExpr` is to
-- (1) flatten sections of the form `PNonRel (PRel x)` into `x`
-- (2) flatten sections of the form `PRel (PNonRel x)` into `x`
-- (3) unwrap any "Rel" with one member and no separators.

simplifyPRel :: PRel -> PRel
simplifyPRel = cata f where
  -- TODO ? Wart: Any `PNonRelF` pattern (here, 2) require recursing by hand.
  -- If I unified PRel and PExpr as an indexed type family,
  -- this clause could be eliminated. c.f. https://www.reddit.com/r/haskell/comments/3sm1j1/how_to_mix_the_base_functorrecursion_scheme_stuff/
  f :: Base PRel PRel -> PRel
  f (PNonRelF (PRel x))      =           simplifyPRel  x
  f (PNonRelF x)             = PNonRel $ simplifyPExpr x
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
