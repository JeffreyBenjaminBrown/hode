{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Hode.Qseq.Valid where

import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Qseq.Types
import Hode.Util.Misc


validProgram :: [(Var,Query e sp)] -> Either String ()
validProgram vqs = do
  (_wholeProgramTest :: ()) <- usesNoSourceBeforeItExists vqs
  let f :: Either String () -> (Var, Query e sp) -> Either String ()
      f e@(Left _) _     = e
      f (Right ()) (_,q) = validQuery q
  foldl f (Right ()) vqs

usesNoSourceBeforeItExists :: forall e sp.
  [(Var,Query e sp)] -> Either String ()
usesNoSourceBeforeItExists vqs = case null bad of
  True -> Right ()
  False -> Left $ "validProgram: variables used before being defined: "
    ++ show bad ++ ".\n"
  where
  (_,bad) = foldl f (S.empty, S.empty) vqs :: (Set Var, Set Var)
  f :: (Set Var, Set Var) -> (Var, Query e sp) -> (Set Var, Set Var)
  f (defined, badAcc) (v,q) =
    -- "defined" are variables defined by previous Queries.
    -- "bad" are variables used before being defined.
    let ext = drawsFromVars q
        moreBad = S.filter (not . flip S.member defined) ext
    in (S.insert v defined, S.union badAcc moreBad)

validQuery :: Query e sp -> Either String ()
validQuery q = prefixLeft "validQuery:" $ do
  usesOnlyIntroducedVars q
  conditionIsVarTestlike q
  noIntroducedVarMasked q
  noAndCollisions q
  feasibleJunctions q

feasibleJunctions :: Query e sp -> Either String ()
feasibleJunctions = recursive where
  simple, recursive :: Query e sp -> Either String ()

  simple (QJunct (QAnd qs)) = let fs = filter findlike qs
    in if null fs
    then Left $ "QAnd with no findlike clauses."
    else Right ()
  simple (QJunct (QOr qs))  =
    let (bs :: [Bool]) = map findlike qs
    in if and bs then Right ()
       else Left $ "In a " ++ show (length qs) ++ "-clause QOr, clause(s) "
            ++ show (map fst $ filter (not . snd) $ zip [1 :: Int ..] bs)
            ++ " are not findlike."
  simple _                  = Right ()

  recursive j@(QJunct w) = do simple j
                              mapM_ recursive $ clauses w
  recursive (QQuant w)   = recursive $ goal w
  recursive _            = Right ()

-- | = Classifying Queries as findlike or testlike

findlike, varTestlike, testlike :: Query e sp -> Bool

findlike (QFind _)                = True
findlike (QJunct (QAnd qs))       = or  $ map findlike qs
findlike (QJunct (QOr  qs@(_:_))) = and $ map findlike qs
findlike (QJunct (QOr  []))       = False
findlike (QQuant w)               =           findlike $ goal w
findlike (QTest _)                = False
findlike (QVTest _)               = False

varTestlike (QVTest _)  = True
varTestlike (QJunct qs) = and $ map varTestlike $ clauses qs
varTestlike (QQuant w)  = varTestlike $ goal w
varTestlike (QTest _)   = False
varTestlike (QFind _)   = False

testlike (QTest _)   = True
testlike (QJunct qs) = and $ map testlike $ clauses qs
testlike (QQuant w)  = testlike $ goal w
testlike (QFind _)   = False
testlike (QVTest _)  = False


-- | = Ensuring the Vars used in a Query make sense

noIntroducedVarMasked :: Query e sp -> Either String ()
noIntroducedVarMasked = f S.empty where
  f :: Set Var -> Query e sp -> Either String ()
  f vs (QQuant w) = do
    let v = name w
        vs' = S.insert v vs
    if elem v vs then Left $ "Var " ++ show v
      ++ " is masked (i.e. by a Var introduced later with the same name)."
      else Right ()
    f vs' $ goal w
  f vs (QJunct j) = mapM_ (f vs) (clauses j)
  f _ _ = Right ()

-- | `noAndCollisions` makes sure that a variable introduced in one
-- clause of a conjunction is never introduced in another clause.
noAndCollisions :: Query e sp -> Either String ()
noAndCollisions (QJunct (QAnd qs)) = do
  let f :: Query e sp -> (Set Var, Either String ())
                      -> (Set Var, Either String ())
      f _ (_, Left s) = (S.empty, Left s) -- short circuit (hence foldr)
      f q (vs, Right ()) =
        let overlap = S.intersection vs $ introducesVars q
        in if S.null overlap
           then ( S.union vs $ introducesVars q, Right () )
           else ( S.empty
                , Left $ "Var used in multiple QAnd clauses: the sets "
                  ++ show vs ++ " and "
                  ++ show (introducesVars q) ++ " overlap." )
  mapM_ (snd . flip f (S.empty, Right ())) qs
  snd (foldr f (S.empty, Right ()) qs)

noAndCollisions (QJunct (QOr qs)) = mapM_ noAndCollisions qs
noAndCollisions (QQuant w)        = noAndCollisions $ goal w
noAndCollisions _                 = Right ()

conditionIsVarTestlike :: Query e sp -> Either String ()
conditionIsVarTestlike (QQuant w) = case varTestlike $ condition w of
  True -> Right ()
  False -> Left $ "In the Quantifier that binds " ++ show (name w)
           ++ " by drawing from " ++ show (source w)
           ++ ", the condition is not varTestlike."
conditionIsVarTestlike _ = Right ()

-- | A Var can only be used (by a Test, VarTest or Find)
-- if it has first been introduced by a ForAll or a ForSome.
usesOnlyIntroducedVars :: Query e sp -> Either String ()
usesOnlyIntroducedVars q0 = prefixLeft "usesOnlyIntroducedVars:"
                            $ f S.empty q0 where
  f :: Set Var -> Query e sp -> Either String ()
  -- The `Set Var` is those Vars that have been introduced so far --
  -- i.e. by the current query or any superquery.

  f vs (QQuant w) = do
    let vs' = S.insert (name w) vs
    f vs' $ condition w
    f vs' $ goal w

  f vs (QJunct j) = ifLefts (map (f vs) $ clauses j)
                    >> Right ()
  f vs q          = if S.isSubsetOf (usesVars q) vs then Right ()
    else Left $ show (usesVars q)
         ++ " not a subset of " ++ show vs

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
  where conditionVars = usesVars $ condition w
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
