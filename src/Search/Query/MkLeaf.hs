{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Search.Query.MkLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Search.Query.Valid
import Search.Subst
import Search.Types
import Util


-- | == Atomic queries. Bigger queries are made of junctions and quantifiers over these.

either_varToElt :: Show e
               => Subst e -> Either e Var -> Either String e
either_varToElt _ (Left e) = Right e
either_varToElt s (Right v) =
  maybe err Right $ M.lookup v s
  where err = Left $ keyErr "either_varToElt" v s


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
  go :: Possible e -> sp -> Subst e -> Either String Bool
  go poss space subst =
    Right $ checkIORel (iVar,iVal) (oVar,oVal) poss
    where
    iVal, oVal :: e
    iVal = maybe (error $ keyErr "varTestIO" iVar poss) id
           $ M.lookup iVar subst
    oVal = maybe (error $ keyErr "varTestIO" oVar poss) id
           $ M.lookup oVar subst

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

  go :: Possible e -> sp -> Subst e -> Either String Bool
  go poss space subst =
    Right $ checkIORel (iInPossible,iVal) (oInPossible,oVal) poss
    where
    iVal, oVal :: e
    iVal = maybe (error $ keyErr "varTestIO" iInSubst poss) id
           $ M.lookup iInSubst subst
    oVal = maybe (error $ keyErr "varTestIO" oInSubst poss) id
           $ M.lookup oInSubst subst

-- | `checkIORel iVar oVar poss subst` determines whether, in poss, iVar
-- is an input that could generate oVar as an output, given
-- their values iVal and oVal.
checkIORel :: forall e sp. (Ord e, Show e)
  => (Var,e) -> (Var,e) -> Possible e -> Bool
checkIORel (iVar,iVal) (oVar,oVal) p = let
  (ce :: CondElts e) =
    maybe (error $ keyErr "varTestIO" oVar p) id
    $ M.lookup oVar p
  (ss :: Set (Subst e)) =
    maybe (error $ keyErr "varTestIO" oVal ce) id
    $ M.lookup oVal ce
  in or $ S.map (M.isSubmapOf $ M.singleton iVar iVal) ss

--varTestIO' :: forall e sp. (Ord e, Show e)
--  => (Var,Var) -> (Var,Var) -> VarTest e sp
--varTestIO' (iInSubst, iInPossible) (oInSubst, oInPossible) =

varTestCompare :: forall e sp. (Eq e, Show e)
  => (e -> e -> Bool) -> Either e Var -> Either e Var
  -> VarTest e sp
varTestCompare compare eev eev' = VarTest go deps where
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']

  go :: Possible e -> sp -> Subst e -> Either String Bool
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
