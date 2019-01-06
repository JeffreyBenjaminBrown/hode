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
couldBind (QUnion     qs) = S.unions $ map couldBind qs
couldBind (QIntersect qs) = S.unions $ map couldBind qs
couldBind (ForSome vfs q) = S.union vs $   couldBind q
  where vs = S.map varFuncName vfs
couldBind (ForAll  _   q) =                couldBind q

-- | Every `QIntersect` must include something `findable`, and
-- every `QUnion` must be nonempty and consist entirely of `findable` things.
findable :: Query -> Bool
findable (QFind _)             = True
findable (QCond _)             = False
findable (QIntersect qs)       = or  $ map findable qs
findable (QUnion     [])       = False
findable (QUnion     qs@(_:_)) = and $ map findable qs
findable (ForSome vfs q)       = findable q
findable (ForAll  _   q)       = findable q

validExistentials :: Query -> Bool
validExistentials (ForSome vfs q)
  = S.disjoint (S.map varFuncName vfs) (couldBind q)
validExistentials (QIntersect qs) = snd $ foldl f (S.empty, True) qs
  where f :: (Set Var, Bool) -> Query -> (Set Var, Bool)
        f (_, False) _ = (S.empty, False) -- short circuit (roughly)
        f (vs, True) q = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
validExistentials _ = True
