{-# LANGUAGE
  ScopedTypeVariables
, TupleSections
#-}

module Search where

import qualified Data.List as L
import           Data.Maybe (isNothing, catMaybes)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Rslt
import Index
import Util


holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
holdsPosition i (r,a) = case positionsIn i a of
  Nothing -> Nothing
  Just ps -> M.lookup r ps

negativeQuery :: Index -> Query -> Addr -> Bool
negativeQuery idx (QNot q) a = not $ elem a $ search idx q
negativeQuery idx (QVariety e) a = Just e == (fst <$> variety idx a)
negativeQuery _ _ _ = error "negativeQuery called for a non-negative query."

joinSubsts :: [Subst] -> Either () Subst
joinSubsts = foldl f (Right M.empty) where
  f :: Either () Subst -> Subst -> Either () Subst
  f (Left ()) _ = Left ()
  f (Right s1) s2 = join2Substs s1 s2

join2Substs :: Subst -> Subst -> Either () Subst
join2Substs m n = case fromM of Left () -> Left ()
                                Right sub -> Right $ M.union sub fromN
 -- `fromM` = the result of unifying each elt of `m` with `n`.
   -- If that unification fails, we never need to find `fromN`.
 -- `fromN` = what is left in `n` that matched with nothing in `m`.
 where fromN = S.foldl f M.empty $ S.difference (M.keysSet n) (M.keysSet m)
         where f :: Subst -> Var -> Subst
               f sub v = M.insert v ((M.!) n v) sub
       fromM = M.foldlWithKey f (Right M.empty) m where
         f :: Either () Subst -> Var -> Addr -> Either () Subst
         f eSub v a = case eSub of
           Left () -> eSub
           Right sub -> case M.lookup v n of
             Just a' | a == a' -> Right $ M.insert v a sub
             Nothing           -> Right $ M.insert v a sub
             _                 -> Left ()

-- TODO next : QHasInRole Role Query, QVar String
-- TODO : test `searchSubst`, once cases that make nonempty `Subst`s are written
-- TODO : `zipWith const` might help for speed.
  -- e.g. because `S.intersection (S.fromList [1]) (S.fromList [1..)) hangs
  -- https://www.reddit.com/r/haskell/comments/9zf1tj/zipwith_const_is_my_favorite_haskell_function/
searchSubst :: Index -> Query -> Map Addr Subst
searchSubst idx q@(QImg _) = M.fromList $ S.toList
                             $ S.map (, M.empty) $ search idx q

searchSubst idx qhr@(QHasInRoles rqs) = M.empty where -- TODO : finish
  (pos,rem)  = L.partition (isPositiveQuery . snd) rqs
  (neg,vars) = L.partition (isNegativeQuery . snd) rem
  (candidates :: Set Addr) = search idx $ QHasInRoles pos

searchSubst idx (QIntersect qs) = S.foldl f M.empty inAll  where
  (inSome :: [Map Addr Subst]) = map (searchSubst idx) qs
  (inAll :: Set Addr) = foldl S.intersection S.empty
                        $ (map M.keysSet inSome :: [Set Addr])
  f :: Map Addr Subst -> Addr -> Map Addr Subst
  f m a = let eSub = joinSubsts $ catMaybes $ map (M.lookup a) inSome
          in case eSub of Left () -> m
                          Right sub -> M.insert a sub m

search :: Index -> Query -> Set Addr
search idx (QImg im) = setFromMaybe $ addrOf idx im
search idx (QIntersect qs) = foldl S.intersection S.empty
  $ map (search idx) qs
search idx (QUnion qs) =     foldl S.union        S.empty
  $ map (search idx) qs

search idx (QHasInRoles rqs) = foldl1 S.intersection lq where
  -- TODO (#fast) Find the smallest set (without evaluating the remainder
  -- of the other sets) and check if the conditions apply to all its members.
  (lq :: [Set Addr]) = map (search idx . uncurry QHasInRole) rqs

search idx (QHasInRole r0 q0) = selectFrom positions where
  positions :: Set (Role, Addr)
  positions = S.foldl S.union S.empty -- Since only one `Expr` fills each position, `S.foldl S.union` destroys no information.
              $ S.map (flat . positionsHeldBy idx)
              $ (search idx q0 :: Set Addr)
    where flat Nothing = S.empty -- Similar to `S.foldl S.union` above.
          flat (Just s) = s
  selectFrom :: Set (Role, Addr) -> Set Addr
  selectFrom = setFromSetOfMaybes . S.map f
    where f :: (Role, Addr) -> Maybe Addr
          f (r,a) = if r==r0 then Just a else Nothing

search idx (QNot _)     = error "Cannot search for a QNot."
search idx (QVariety _) = error "Cannot search for a QVariety."
search idx (QVar _)     = error "Cannot search for a QVar."
