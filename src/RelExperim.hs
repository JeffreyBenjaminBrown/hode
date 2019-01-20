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


type Rel = R.Relation
type CondElts' e = Rel e (Subst e)
type Possible' e = Map Var (CondElts' e)

data Tource = Tource  { retrieving :: [Var]
                      , inputs     :: (Set Var)
                      , outputs    :: (Set Var) }

-- | If s binds i to i0 and o to o0, calling
-- `varPossibilities p s (Tource v (S.singleton i) (S.singleton o))`,
-- should yield all values of v for which i=i0 is an input and for which
-- o=o0 is an output.

varPossibilities :: forall e. (Ord e, Show e)
                 => Possible e -> Subst e -> Tource
                 -> Either String (Possible e)
varPossibilities    p           s        (Tource vs ins outs)

  | S.null ins && S.null outs = let
      f :: Var -> Either String (CondElts e)
      f v = maybe (Left $ keyErr "varPossibilities" v p) Right
            $ M.lookup v p
      se, lefts :: [Either String (CondElts e)]
      se = map f vs
      lefts = filter isLeft se
      in case null lefts of
      False -> Left $ foldr (++) "" $ map (fromLeft "") lefts
      True -> Right $ M.fromList $ zip vs $ map (fromRight mempty) se

--  | S.null ins = let
--    (pRestricted :: Possible e) = M.map f $ M.restrictKeys p outs
--      -- Any binding in a Subst in p that mentions a variable other than
--      -- v or one of the dets is irrelevant. So is any such key of p.
--      where
--        f :: CondElts e -> CondElts e
--        f = M.map $ S.map $ flip M.restrictKeys $ S.singleton v
--
--    in case reconcileDetsAcrossVars pRestricted s outs of
--    Left s -> Left $ "varPossibilities: error in callee:\n" ++ s
--    -- This only yields Left is if inputSubsts gives a lookup error.
--    -- Finding the empty set of `Subst`s is acceptable.
--
--    Right (substs :: Set (Subst e)) -> let
--      (possible :: Set e) = M.keysSet $ setSubstToCondElts v substs
--      (mce_v :: Maybe (CondElts e)) = M.lookup v p
--      in maybe (Left $ keyErr "varPossibilities" v p)
--         (Right . flip M.restrictKeys possible) mce_v
--
