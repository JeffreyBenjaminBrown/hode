{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Qseq.MkLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.Subst
import Qseq.QTypes
import Util


either_varToElt :: Show e
               => Subst e -> Either e Var -> Either String e
either_varToElt _ (Left e) = Right e
either_varToElt s (Right v) =
  maybe err Right $ M.lookup v s
  where err = Left $ keyErr "either_varToElt" v s


-- | == Atomic queries. Bigger queries are made of junctions and quantifiers over these.

-- | = `Test`s

mkTest :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Test e sp
mkTest compare eev = Test go deps where
  go :: sp -> Subst e -> e -> Either String Bool
  go _ s e = do e0 <- either_varToElt s eev
                Right $ compare e0 e
  deps :: Set Var
  deps = S.fromList $ catMaybes $ map (either (const Nothing) Just) [eev]


-- | = `VarTest`s

-- | `mkVTestIO iVar oVar` creates a `VarTest` that returns `True`
-- if and only if the "current" (bound according to the `Subst` argument
-- to `runVarTest`) value of iVar could have led to the current value of oVar.
-- (abbrevations: i=input, o=output.)
mkVTestIO :: forall e sp. (Ord e, Show e)
  => Var -> Var -> VarTest e sp
mkVTestIO iVar oVar = VarTest go deps where
  (deps :: Set Var) = S.fromList [iVar,oVar]
  go :: sp -> Possible e -> Subst e -> Either String Bool
  go space poss subst = do
    iVal <- maybe (Left $ keyErr "mkVTestIO" iVar subst) Right
            $ M.lookup iVar subst
    oVal <- maybe (Left $ keyErr "mkVTestIO" oVar subst) Right
            $ M.lookup oVar subst
    _checkIORel (iVar,iVal) (oVar,oVal) poss

-- | `mkVTestIO'` is like `mkVTestIO`, but takes into account the fact
-- that the name used (i.e. the `Var`) in the `Subst` might differ from
-- the corresponding name in the `Possible`.
--
-- PITFALL : mkVTestIO' : naming error => silent failure 
-- inPossible is the name used as an input to oInPossible;
-- it is not (at least in general) itself a key to the Possible.
--
-- Examples:
--   See Test.TProgram.test_runNestedQuants for a fully worked example.
--   Here's a sketch of the idea: Suppose the `Possible` is
--     [ (a, fromlist [ (1, empty)
--                    , (2, empty) ] )
--     , (b, fromlist [ ( 1, S.singleton $ M.singleton a1 1 ) ] ) ]
--   Then when trying to restrict to a-b pairs such that a is an
--   input to b, we must call mkVTestIO' something like this:
--     mkVTestIO' (a2,a1) (b2,b)
--   and not, though it unfortunately seems more natural, this:
--     mkVTestIO' (a2,a) (b2,b)

mkVTestIO' :: forall e sp. (Ord e, Show e)
  => (Var,Var) -- ^ name of the input  in Subst and Possible, resp.
  -> (Var,Var) -- ^ name of the output in Subst and Possible, resp.
  -> VarTest e sp
mkVTestIO' (iInSubst, iInPossible) (oInSubst, oInPossible) =
  VarTest go deps where
  (deps :: Set Var) = S.fromList [iInSubst, oInSubst]

  go :: sp -> Possible e -> Subst e -> Either String Bool
  go space poss subst = do
    iVal <- maybe (Left $ keyErr "mkVTestIO'" iInSubst subst) Right
           $ M.lookup iInSubst subst
    oVal <- maybe (Left $ keyErr "mkVTestIO'" oInSubst subst) Right
           $ M.lookup oInSubst subst
    _checkIORel (iInPossible,iVal) (oInPossible,oVal) poss

-- | `_checkIORel iVar oVar poss subst` determines whether, in poss, iVar
-- is an input that could generate oVar as an output, given
-- their values iVal and oVal.
_checkIORel :: forall e sp. (Ord e, Show e)
  => (Var,e) -> (Var,e) -> Possible e -> Either String Bool
_checkIORel (iVar,iVal) (oVar,oVal) p = do
  (ce :: CondElts e) <-
    maybe (Left $ keyErr "_checkIORel: key not in Possible" oVar p) Right
    $ M.lookup oVar p
  let (ss :: Set (Subst e)) = maybe S.empty id $ M.lookup oVal ce
  Right $ or $ S.map (M.isSubmapOf $ M.singleton iVar iVal) ss

mkVTestCompare :: forall e sp. (Eq e, Show e)
  => (e -> e -> Bool) -> Either e Var -> Either e Var
  -> VarTest e sp
mkVTestCompare compare eev eev' = VarTest go deps where
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']

  go :: sp -> Possible e -> Subst e -> Either String Bool
  go _ _ s = do
    (e  :: e) <- prefixLeft "mkVTestCompare" $ either_varToElt s eev
    (e' :: e) <- prefixLeft "mkVTestCompare" $ either_varToElt s eev'
    Right $ compare e e'


-- | = `Find`s

mkFindReturn :: forall e sp. (Show e)
             => Either e Var -> Find e sp
mkFindReturn e = Find f deps where
  f :: sp -> Subst e -> Either String (Set e)
  f _ s = either_varToElt s e >>= Right . S.singleton
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [e]

mkFindReturn' :: forall e sp. (Show e)
             => Set e -> Find e sp
mkFindReturn' se = Find f S.empty where
  f :: sp -> Subst e -> Either String (Set e)
  f _ _ = Right se

-- | Run a search that does not depend on the `Subst`.
mkFind :: forall e sp. (Show e)
  => (sp -> Either String (Set e))
  -> Find e sp
mkFind finder = Find go S.empty where
  go :: sp -> Subst e -> Either String (Set e)
  go g _ = finder g

-- | Run a search starting from a variable element of the space.
mkFindFrom :: forall e sp. (Show e) =>
  (sp -> e -> Either String (Set e))
  -> Either e Var -> Find e sp
mkFindFrom finder eev = Find go deps where
  go :: sp -> Subst e -> Either String (Set e)
  go g s = either_varToElt s eev >>= finder g
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev]
