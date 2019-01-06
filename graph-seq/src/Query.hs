module Query where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


-- | `couldBind Q = Vs` <=> `Q` could depend on a binding of any var in `Vs`.
-- `willBind` would be a nice thing to define if it were possible, but
-- (without way more data and processing) it is not.
couldBind :: Query -> Set Var
couldBind (QFind _) = S.empty
couldBind (QCond _) = S.empty
couldBind (QUnion     qs) = S.unions   $ S.map couldBind qs
couldBind (QIntersect qs) = S.unions   $ S.map couldBind qs
couldBind (ForSome vfs q) = S.union vs $       couldBind q
  where vs = S.map varFuncName vfs
couldBind (ForAll  _   q) =                    couldBind q

-- | Every `QIntersect` must include something `findable`, and
-- every `QUnion` must consist entirely of `findable` things.
findable :: Query -> Bool
findable (QFind _)       = True
findable (QCond _)       = False
findable (QIntersect qs) = S.foldl (||) False $ S.map findable qs
findable (QUnion     qs) = S.foldl (&&) True  $ S.map findable qs
findable (ForSome vfs q) = findable q
findable (ForAll  _   q) = findable q

validExistentials :: Query -> Bool
validExistentials (ForSome vfs q)
  = S.disjoint (S.map varFuncName vfs) (couldBind q)
validExistentials (QIntersect qs) = snd $ S.foldl f (S.empty, True) qs
  where f :: (Set Var, Bool) -> Query -> (Set Var, Bool)
        f (_, False) _ = (S.empty, False)
        f (vs, True) q = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
  -- If that doesn't work, here's a (less efficient) definition:
  -- validExistentials q@(QIntersect qs) = size == sizes where
  -- -- Inefficient, because it won't short-circuit.
  -- size  = S.size $ couldBind q
  -- sizes = S.foldl (+) 0 $ S.map (S.size . couldBind) qs
validExistentials _ = True
