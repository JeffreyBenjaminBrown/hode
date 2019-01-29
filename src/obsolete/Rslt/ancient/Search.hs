{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Rslt.Search where

import qualified Data.List as L
import           Data.Maybe (isNothing, catMaybes)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Data.Rslt
import Data.Rslt.Index
import Util


newtype Var = Var String -- ^ a variable, in the logic programming sense
  deriving (Show, Eq, Ord)

type RSubst = M.Map Var Addr -- TODO ? replace `Addr` with `Either Var Addr`

data RQuery = RQImg ImgOfExpr
   |  RQHasInRole Role RQuery
   |  RQHasInRoles [(Role, RQuery)]
   |  RQIntersect [RQuery]  |  RQUnion [RQuery]
   |  RQNot RQuery  |  RRQVariety ExprCtr
   |  RQVar String

-- | Positive `RQuery`s are things you can search for.
-- Non-positive `RQuery`s are too broad to search for.
-- `RQVar` is neither positive nor negative.
isPositiveRQuery, isNegativeRQuery, isVariableRQuery :: RQuery -> Bool
isPositiveRQuery (RQImg _)         = True
isPositiveRQuery (RQHasInRole _ _) = True
isPositiveRQuery (RQHasInRoles _)  = True
isPositiveRQuery (RQIntersect _)   = True
isPositiveRQuery (RQUnion _)       = True
isPositiveRQuery _                = False
-- Will I need a recursive version?
  --isPositiveRQuery (RQHasInRole _ q)  =            isPositiveRQuery q
  --isPositiveRQuery (RQHasInRoles qrs) = or  $ map (isPositiveRQuery . snd) qrs
  --isPositiveRQuery (RQIntersect qs)   = or  $ map  isPositiveRQuery        qs
  --isPositiveRQuery (RQUnion qs)       = and $ map  isPositiveRQuery        qs

isNegativeRQuery (RQNot _)     = True
isNegativeRQuery (RRQVariety _) = True
isNegativeRQuery _            = False

isVariableRQuery (RQVar _) = True
isVariableRQuery _        = False

negativeRQuery :: Index -> RQuery -> Addr -> Bool
negativeRQuery idx (RQNot q) a = not $ elem a $ search idx q
negativeRQuery idx (RRQVariety e) a = Just e == (fst <$> variety idx a)
negativeRQuery _ _ _ = error "negativeRQuery called for a non-negative query."

joinRSubsts :: [RSubst] -> Either () RSubst
joinRSubsts = foldl f (Right M.empty) where
  f :: Either () RSubst -> RSubst -> Either () RSubst
  f (Left ()) _ = Left ()
  f (Right s1) s2 = join2RSubsts s1 s2

join2RSubsts :: RSubst -> RSubst -> Either () RSubst
join2RSubsts m n = case fromM of Left () -> Left ()
                                 Right sub -> Right $ M.union sub fromN
 -- `fromM` = the result of unifying each elt of `m` with `n`.
   -- If that unification fails, we never need to find `fromN`.
 -- `fromN` = what is left in `n` that matched with nothing in `m`.
 where fromN = S.foldl f M.empty $ S.difference (M.keysSet n) (M.keysSet m)
         where f :: RSubst -> Var -> RSubst
               f sub v = M.insert v ((M.!) n v) sub
       fromM = M.foldlWithKey f (Right M.empty) m where
         f :: Either () RSubst -> Var -> Addr -> Either () RSubst
         f eSub v a = case eSub of
           Left () -> eSub
           Right sub -> case M.lookup v n of
             Just a' | a == a' -> Right $ M.insert v a sub
             Nothing           -> Right $ M.insert v a sub
             _                 -> Left ()

-- TODO next : RQHasInRole Role RQuery, RQVar String
-- TODO : test `searchRSubst`, once cases that make nonempty `RSubst`s are written
-- TODO : `zipWith const` might help for speed.
  -- e.g. because `S.intersection (S.fromList [1]) (S.fromList [1..)) hangs
  -- https://www.reddit.com/r/haskell/comments/9zf1tj/zipwith_const_is_my_favorite_haskell_function/
searchRSubst :: Index -> RQuery -> Map Addr RSubst
searchRSubst idx q@(RQImg _) = M.fromList $ S.toList
                             $ S.map (, M.empty) $ search idx q

searchRSubst idx qhr@(RQHasInRoles rqs) = M.empty where -- TODO : finish
  (pos,rem)  = L.partition (isPositiveRQuery . snd) rqs
  (neg,vars) = L.partition (isNegativeRQuery . snd) rem
  (candidates :: Set Addr) = search idx $ RQHasInRoles pos

searchRSubst idx (RQIntersect qs) = S.foldl f M.empty inAll  where
  (inSome :: [Map Addr RSubst]) = map (searchRSubst idx) qs
  (inAll :: Set Addr) = foldl S.intersection S.empty
                        $ (map M.keysSet inSome :: [Set Addr])
  f :: Map Addr RSubst -> Addr -> Map Addr RSubst
  f m a = let eSub = joinRSubsts $ catMaybes $ map (M.lookup a) inSome
          in case eSub of Left () -> m
                          Right sub -> M.insert a sub m

search :: Index -> RQuery -> Set Addr
search idx (RQImg im) = maybe S.empty S.singleton $ addrOf idx im
search idx (RQIntersect qs) = foldl S.intersection S.empty
  $ map (search idx) qs
search idx (RQUnion qs) =     foldl S.union        S.empty
  $ map (search idx) qs

search idx (RQHasInRoles rqs) = foldl1 S.intersection lq where
  -- TODO (#fast) Find the smallest set (without evaluating the remainder
  -- of the other sets) and check if the conditions apply to all its members.
  (lq :: [Set Addr]) = map (search idx . uncurry RQHasInRole) rqs

search idx (RQHasInRole r0 q0) = selectFrom positions where
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

search idx (RQNot _)     = error "Cannot search for a RQNot."
search idx (RRQVariety _) = error "Cannot search for a RRQVariety."
search idx (RQVar _)     = error "Cannot search for a RQVar."
