{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module SeekSeq.Query.MkLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import SeekSeq.Query.Valid
import SeekSeq.Subst
import SeekSeq.Types
import Util


either_varToElt :: Show e
               => Subst e -> Either e Var -> Either String e
either_varToElt _ (Left e) = Right e
either_varToElt s (Right v) =
  maybe err Right $ M.lookup v s
  where err = Left $ keyErr "either_varToElt" v s


-- | == Atomic queries. Bigger queries are made of junctions and quantifiers over these.

-- | = `Test`s

test :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Test e sp
test compare eev = Test go deps where
  go :: sp -> Subst e -> e -> Either String Bool
  go _ s e = do e0 <- either_varToElt s eev
                Right $ compare e0 e
  deps :: Set Var
  deps = S.fromList $ catMaybes $ map (either (const Nothing) Just) [eev]


-- | = `VarTest`s

-- | `varTestIO iVar oVar` creates a `VarTest` that returns `True`
-- if and only if the "current" (bound according to the `Subst` argument
-- to `runVarTest`) value of iVar could have led to the current value of oVar.
-- (abbrevations: i=input, o=output.)
varTestIO :: forall e sp. (Ord e, Show e)
  => Var -> Var -> VarTest e sp
varTestIO iVar oVar = VarTest go deps where
  (deps :: Set Var) = S.fromList [iVar,oVar]
  go :: sp -> Possible e -> Subst e -> Either String Bool
  go space poss subst = do
    iVal <- maybe (Left $ keyErr "varTestIO" iVar subst) Right
            $ M.lookup iVar subst
    oVal <- maybe (Left $ keyErr "varTestIO" oVar subst) Right
            $ M.lookup oVar subst
    checkIORel (iVar,iVal) (oVar,oVal) poss


-- | `varTestIO'` is like `varTestIO`, but takes into account the fact
-- that the name used (i.e. the `Var`) in the `Subst` might differ from
-- the corresponding name in the `Possible`.
varTestIO' :: forall e sp. (Ord e, Show e)
  => (Var,Var) -- ^ name of the input  in Subst and Possible, resp.
  -> (Var,Var) -- ^ name of the output in Subst and Possible, resp.
  -> VarTest e sp
varTestIO' (iInSubst, iInPossible) (oInSubst, oInPossible) =
  VarTest go deps where
  (deps :: Set Var) = S.fromList [iInSubst, oInSubst]

  go :: sp -> Possible e -> Subst e -> Either String Bool
  go space poss subst = do
    iVal <- maybe (Left $ keyErr "varTestIO'" iInSubst subst) Right
           $ M.lookup iInSubst subst
    oVal <- maybe (Left $ keyErr "varTestIO'" oInSubst subst) Right
           $ M.lookup oInSubst subst
    checkIORel (iInPossible,iVal) (oInPossible,oVal) poss

-- | `checkIORel iVar oVar poss subst` determines whether, in poss, iVar
-- is an input that could generate oVar as an output, given
-- their values iVal and oVal.
checkIORel :: forall e sp. (Ord e, Show e)
  => (Var,e) -> (Var,e) -> Possible e -> Either String Bool
checkIORel (iVar,iVal) (oVar,oVal) p = do
  (ce :: CondElts e) <-
    maybe (Left $ keyErr "checkIORel: key not in Possible" oVar p) Right
    $ M.lookup oVar p
  let (ss :: Set (Subst e)) = maybe S.empty id $ M.lookup oVal ce
  Right $ or $ S.map (M.isSubmapOf $ M.singleton iVar iVal) ss

varTestCompare :: forall e sp. (Eq e, Show e)
  => (e -> e -> Bool) -> Either e Var -> Either e Var
  -> VarTest e sp
varTestCompare compare eev eev' = VarTest go deps where
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']

  go :: sp -> Possible e -> Subst e -> Either String Bool
  go _ _ s = do
    (e  :: e) <- prefixLeft "varTestCompare" $ either_varToElt s eev
    (e' :: e) <- prefixLeft "varTestCompare" $ either_varToElt s eev'
    Right $ compare e e'


-- | = `Find`s

-- | Run a search that does not depend on the `Subst`.
find :: forall e sp. (Show e) => (sp -> Set e) -> Find e sp
find finder = Find go S.empty where
  go :: sp -> Subst e -> Either String (Set e)
  go g _ = Right $ finder g

-- | Run a search starting from some element of the space.
findFrom :: forall e sp. (Show e) =>
  String -> (sp -> e -> Set e) -> Either e Var -> Find e sp
findFrom findName finder eev = Find go deps where
  go :: sp -> Subst e -> Either String (Set e)
  go g s = either_varToElt s eev >>= Right . finder g
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev]
