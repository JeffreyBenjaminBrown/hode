{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Query.MkLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Query.Inspect
import Subst
import Types
import Util


-- | == Atomic queries. Bigger queries are made of junctions and quantifiers over these.

unEitherEltVar :: Subst e -> Either e Var -> (Maybe e, Maybe Var)
unEitherEltVar _ (Left e) = (Just e, Nothing)
unEitherEltVar s (Right v) = (M.lookup v s, Just v)


-- | = `Test`s

test :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Test e sp
test compare eev = Test go deps where
  go :: sp -> Subst e -> e -> Bool
  go _ s = let (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
               err = error $ keyErr "test" (fromJust mv) s
           in maybe err compare me
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
  go :: Possible e -> sp -> Subst e -> Bool
  go p _ s = let
    iVal, oVal :: e
    oVal = maybe (error $ keyErr "varTestIO" oVar p) id
           $ M.lookup oVar s
    iVal = maybe (error $ keyErr "varTestIO" iVar p) id
           $ M.lookup iVar s

    (ce :: CondElts e) =
      maybe (error $ keyErr "varTestIO" oVar p) id
      $ M.lookup oVar p
    (ss :: Set (Subst e)) =
      maybe (error $ keyErr "varTestIO" oVal ce) id
      $ M.lookup oVal ce
    in or $ S.map (M.isSubmapOf $ M.singleton iVar iVal) ss

varTestCompare :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Either e Var -> VarTest e sp
varTestCompare compare eev eev' = VarTest go deps where
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']

  go :: Possible e -> sp -> Subst e -> Bool
  go _ _ s = let
    (me , mv ) = unEitherEltVar s eev  :: (Maybe e, Maybe Var)
    (me', mv') = unEitherEltVar s eev' :: (Maybe e, Maybe Var)
    err v = error $ keyErr "varTestCompare" (fromJust v) s
    in case me of
         Nothing -> err mv
         Just e -> case me' of Nothing -> err mv'
                               Just e' -> compare e e'


-- | = `Find`s

-- | Run a search that does not depend on the `Subst`.
find :: forall e sp. (Show e) =>
  String -> (sp ->      Set e) ->                 Find e sp
-- | Run a search starting from some element of the space.
findFrom :: forall e sp. (Show e) =>
  String -> (sp -> e -> Set e) -> Either e Var -> Find e sp

find findName finder = Find go S.empty where
  go :: sp -> Subst e -> Set e
  go g _ = finder g
findFrom findName finder eev = Find go deps where
  go :: sp -> Subst e -> Set e
  go g s = maybe err (finder g) me where
      (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
      err = error $ keyErr findName (fromJust mv) s
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev]
