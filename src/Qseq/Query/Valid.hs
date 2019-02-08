{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Qseq.Query.Valid where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes
import Util


validProgram :: [(Var,Query e sp)] -> Either String ()
validProgram vqs = do
  (_wholeProgramTest :: ()) <- usesNoSourceBeforeItExists vqs
  let f :: Either String () -> (Var, Query e sp) -> Either String ()
      f e@(Left _) _     = e
      f (Right ()) (_,q) = validQuery q
  foldl f (Right ()) vqs

usesNoSourceBeforeItExists :: forall e sp. [(Var,Query e sp)] -> Either String ()
usesNoSourceBeforeItExists vqs = case null bad of
  True -> Right ()
  False -> Left $ "validProgram: variables used before being defined: "
    ++ show bad ++ ".\n"
  where
  (_,bad) = foldl f (S.empty, S.empty) vqs :: (Set Var, Set Var)
  f :: (Set Var, Set Var) -> (Var, Query e sp) -> (Set Var, Set Var)
  f (defined,bad) (v,q) =
    -- "defined" are variables defined by previous Queries.
    -- "bad" are variables used before being defined.
    let ext = drawsFromVars q
        moreBad = S.filter (not . flip S.member defined) ext
    in (S.insert v defined, S.union bad moreBad)

validQuery :: Query e sp -> Either String ()
validQuery q = do
  if usesOnlyIntroducedVars q then Right ()
    else Left $ "Query uses a Var before introducing it."
  if noAndCollisions q then Right ()
    else Left $ "Variable defined in multiple clauses of a conjunction."
  if noIntroducedVarMasked q then Right ()
    else Left $ "One variable definition masks another."
  if feasibleJunctions q then Right ()
    else Left $ "Infeasible junction in Query."
  if null $ S.intersection (introducesVars q) (drawsFromVars q) then Right ()
    -- introducesVars and drawsFromVars are recursive, so this needn't be.
    else Left $ "Names shared between internally-defined and externally-drawn-from variables."

feasibleJunctions :: Query e sp -> Bool
feasibleJunctions = recursive where
  simple, recursive :: Query e sp -> Bool

  simple (QJunct (QAnd qs)) = not $ null $ filter findlike qs
  simple (QJunct (QOr qs))  = and $           map findlike qs
  simple _                  = True

  recursive j@(QJunct w) = simple j && and (map recursive $ clauses w)
  recursive (QQuant w)   =                      recursive $ goal w
  recursive _            = True


-- | = Classifying Queries as findlike or testlike

findlike, varTestlike, testlike :: Query e sp -> Bool

findlike (QFind _)                = True
findlike (QJunct (QAnd qs))       = or  $ map findlike qs
findlike (QJunct (QOr  qs@(_:_))) = and $ map findlike qs
findlike (QQuant w)               =           findlike $ goal w
findlike _                        = False

varTestlike (QVTest _)  = True
varTestlike (QJunct qs) = and $ map varTestlike $ clauses qs
varTestlike (QQuant w)  = varTestlike $ goal w
varTestlike _           = False

testlike q = not (findlike q || varTestlike q)


-- | = Ensuring the Vars used in a Query make sense

noIntroducedVarMasked :: Query e sp -> Bool
noIntroducedVarMasked = f S.empty where
  f :: Set Var -> Query e sp -> Bool
  f vs (QQuant w) = let v = name w
                        vs' = S.insert v vs
                    in not (elem v vs) && f vs' (goal w)
  f vs (QJunct j) = and $ map (f vs) $ clauses j
  f _ _ = True

-- | `noAndCollisions` makes sure that a variable introduced in one
-- clause of a conjunction is never introduced in another clause.
noAndCollisions :: Query e sp -> Bool
noAndCollisions (QJunct (QAnd qs)) =
  and (map noAndCollisions qs)
  && snd (foldr f (S.empty, True) qs)
  where
    f :: Query e sp -> (Set Var, Bool) -> (Set Var, Bool)
    f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
    f q (vs, True) = if S.disjoint vs $ introducesVars q
                     then (S.union vs $ introducesVars q, True)
                     else (S.empty                      , False)
noAndCollisions (QJunct (QOr qs)) =
  and (map noAndCollisions qs)
noAndCollisions (QQuant w) = noAndCollisions $ goal w
noAndCollisions _ = True

-- | A Var can only be used (by a Test, VarTest or Find)
-- if it has first been introduced by a ForAll or a ForSome.
usesOnlyIntroducedVars :: Query e sp -> Bool
usesOnlyIntroducedVars q = f S.empty q where
  f :: Set Var -> Query e sp -> Bool
  -- The `Set Var` is those Vars that have been introduced so far --
  -- i.e. by the current query or any superquery.

  f vs (QQuant w) = okConditions && f vs' (goal w)
    where okConditions = and $ map (f vs') $ conditions w
          vs' = S.insert (name w) vs
  f vs (QJunct j) = and $ map (f vs) $ clauses j
  f vs _          = S.isSubsetOf (usesVars q) vs

-- | `drawsFromVars q` returns the set of all variables that q expects
-- to have been defined by previous `Query`s. It uses these as sources
-- from which to define local variables.
drawsFromVars :: Query e sp -> Set Var
drawsFromVars (QJunct j) = S.unions $ map drawsFromVars $ clauses j
drawsFromVars (QQuant q) = S.insert (source q) $ drawsFromVars $ goal q
drawsFromVars _ = S.empty

-- | `usesVars q` returns the union of all the `Vars` uses by the atomic
-- queries ("leaves") of q -- that is, the `VarTest`s, `Test`s and `Find`s.
usesVars :: Query e sp -> Set Var
usesVars (QQuant w) = S.union (usesVars $ goal w) conditionVars
  where conditionVars = S.unions $ map usesVars $ conditions w
usesVars (QJunct j) = S.unions $ map usesVars $ clauses j
usesVars (QVTest x@(VarTest _ _)) = varTestUses x
usesVars (QTest  x@(Test    _ _)) = testUses    x
usesVars (QFind  x@(Find    _ _)) = findUses    x

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, introduces the `Var`
-- "v". And every `Query` introduces whatever its subqueries introduce.
introducesVars :: Query e sp -> Set Var
introducesVars (QJunct j) = S.unions $ map      introducesVars $ clauses j
introducesVars (QQuant w) = S.insert (name w) $ introducesVars $ goal w
introducesVars _          = S.empty
