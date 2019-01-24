{-# LANGUAGE ScopedTypeVariables #-}

module RelExperim where

import           Prelude
import           Data.Either
import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import qualified Data.Relation  as R
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types
import Util


-- TODO ? use Rel, CondElts', Possible'
type Rel = R.Relation
type CondElts' e = Rel e (Subst e)
type Possible' e = Map Var (CondElts' e)

-- TODO A TSource t is valid only if
-- (1) The length of `haveInputs t`  matches the length of
--    `namesItsInputs p`     for each `p` in `plans t`.
-- (2) The length of `haveOutputs t` matches the length of
--   `isNamedByItsOutputs p` for each `p` in `plans t`.
-- (Also the two lists should correspond, but the code can't check that
-- without reading the user's mind.)

data TSource = TSource {
    plans       :: [TVarPlan] -- ^ new `Var`s to introduce in this `Query`
  , haveInputs  :: [Var] -- ^ only values for which these `Var`s
    -- (as dictated by some `Subst`) are inputs will be returned
  , haveOutputs :: [(Var,Var)] -- ^ Each fst is a source (a key in the
    -- Possible), and each snd is the corresponding `Var` in the `Subst`.
  } deriving (Show, Eq, Ord)

data TVarPlan = TVarPlan {
    tSource :: Var -- ^ where in the `Possible` values are drawn from
  , tName   :: Var -- ^ what the drawn values will be called in this `Query`
  , namesItsInputs      :: [Var]
  , isNamedByItsOutputs :: [Var]
  } deriving (Show, Eq, Ord)

-- | Consider `varPossibilities p s src`, where
--   s = M.fromList [(i1,iVal), (o1,oVal)]
--   `src = TSource vp [i1] [(o,o1)]`
--   `vp = TVarPlan "a" "a2" ["i"] ["a1"]`.
--
-- That says:
--   a and o are the names of the results of two previous Queries.
--   When a was created, it relied on an input it called i.
--     (To specify a second such input, make a variable like i, called say j
--     (and j1, and jVal, etc.))
--   When o was created, it relied on an input it called a1.
--     (To specify a second output, make another variable like o, called say n
--     (and n1, and nVal, etc.))
--   We want:
--     to draw values (call them aVal) from the results for a,
--       (To specify another var to bind under the same conditions,
--       make a variable like a, say b, and b1, b2, bVal, etc.)
--     to bind the drawn value aVal to the name a2,
--     and to include only those values of aVal such that:
--       i=iVal was an input to the binding a=aVal.
--       a1=aVal was an input to the binding o=oVal.
--
-- (The numbers indicate the sequence in which references were created.
-- For instance, when a was created, it was called a. Then that set was
-- drawn from in order to create o; the draws were called a1. Now we
-- want to draw from it again, and call those draws a2. Actual calls do
-- not have to follow that convention.)
--
-- Procedure: First, create a set of candidates for a2 by matching inputs:
-- Find a in the `Possible`, then restrict each Set Subst in that CondElts
-- to include only `Subst`s that include the binding i=i1v. The keys of
-- the resulting CondElts are our candidate values for a2.
--
-- Next we filter those results by matching outputs.
-- Consider a particular candidate, a2=aVal.
-- aVal meets the output-matching criteria if o uses a1=aVal as an input.
-- We find the CondElts keyed by "o" in the Possible,
-- and within that CondElts we look up oVal.
-- If any Subst in the resulting Set Subst contains the binding
-- a1=aVal, then aVal survives.

--outputs_use_candidate :: forall e. (Ord e, Show e)
--                      => Possible e -> Subst e -> TSource -> TVarPlan
--                      -> e
--                      -> Either String
--                        ([Var], [Var], Subst e, Subst e
--                        , CondElts e, Set (Subst e), Bool)
--                      -- -> Either String Bool
--
--outputs_use_candidate p s src pl e = do
--  let (outsInPossible  :: [Var])   = map fst $ haveOutputs src
--      (outsInSubst     :: [Var])   = map snd $ haveOutputs src
--      (substRestricted :: Subst e) = M.restrictKeys s $ S.fromList outsInSubst
--  (presentCondition    :: Subst e) <-
--    ifLefts_mapKeys "outputs_use_candidate: error in callee:\n"
--    $ M.mapKeys (renameOut src) substRestricted
--  (outputCes :: [CondElts e]) <-
--    maybe (Left $ keyErr "outputs_use_candidate" (tSource pl) p) Right
--    $ map (flip M.lookup p) outsInPossible
--  (couldHaveLedHere :: [Set (Subst e)]) <-
--    ifLefts "outputs_use_candidate"
--    $ map (uncurry M.lookup) $ zip outsInSubst outputCes
--  let result = any (M.isSubmapOf presentCondition) feasibleConditions
--  Right $ ( outsInPossible, outsInSubst, substRestricted, presentCondition
--          , ce, feasibleConditions, result )

input_matched_varPossibilities' :: forall e. (Ord e, Show e)
                                => Possible e -> Subst e -> TSource
                                -> Either String (Possible e)
input_matched_varPossibilities'    p           s            t = do
  let pls = plans t -- TODO ? I don't need haveInputs?
  (sources :: [CondElts e]) <-
    let (fetchFromPossible :: Var -> Either String (CondElts e)) =
          \v -> maybe (Left $ keyErr "fetch" v p) Right $ M.lookup v p
    in ifLefts "input_matched_varPossibilities': error in callee:\n"
       $ map (fetchFromPossible . tSource) pls

  (sources_ins_matched :: [CondElts e]) <-
    let restrict :: (TVarPlan, CondElts e) -> Either String (CondElts e)
        restrict (pl, ce) = restrictToMatchIns t pl s ce
    in ifLefts "input_matched_varPossibilities': error in callee:\n"
       $ map restrict $ zip pls sources
  Right $ M.fromList $ zip (map tName pls) sources_ins_matched

restrictToMatchIns :: forall e. Eq e =>
  TSource -> TVarPlan -> Subst e
  -> CondElts e -> Either String (CondElts e)

restrictToMatchIns t pl s ce = do
  let ins = haveInputs t
      subst_ins = M.restrictKeys s $ S.fromList ins :: Subst e
  (subst_ins_renamed :: Subst e) <-
    ifLefts_mapKeys "insMatch: error in callee:\n"
    $ M.mapKeys (renameIn t pl) subst_ins
  let (supermaps :: CondElts e) =
        M.map ( S.filter $ M.isSubmapOf subst_ins_renamed) ce
  Right $ M.filter (not . S.null) $ supermaps

-- | The `TSource` contains a list of the inputs from the current `Subst`
-- being used. The `TVarPlan` has a list of what the source named in
-- that plan calls its inputs. This takes a `Var` that should be in the
-- first list (and the `Subst`), and renames it using the second list.
renameIn :: TSource -> TVarPlan -> Var -> Either String Var
renameIn t pl v = let
  renamer = M.fromList $ zip ins newNames
    where ins = haveInputs t
          newNames = namesItsInputs pl
  in maybe (Left $ keyErr "renameInput" v renamer) Right
     $ M.lookup v renamer

-- | The `TSource` contains a list of pairs, the fst of which is a source
-- in the `Possible`, and the snd of which is the `Var` in the `Subst`
-- playing that role. This renames a `Var` from one of those snd elements
-- (which should also be in the `Subst`), and renames it to the coresponding
-- fst.
renameOut :: TSource -> Var -> Either String Var
renameOut t v = let
  renamer = M.fromList $ map swap pairs
    where pairs = haveOutputs t
          swap (a,b) = (b,a)
  in maybe (Left $ keyErr "renameOut" v renamer) Right
     $ M.lookup v renamer
