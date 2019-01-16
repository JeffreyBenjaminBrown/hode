{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Subst where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Types
import Util


-- | = unifying "Solutions", that is, `(Set (e, Subst e))`s

unifySolutions :: forall e. (Ord e) => Set (Set (e, Subst e))
                                    ->      Set (e, Subst e)
unifySolutions = setFoldl1 unifySolutions_2_sets

unifySolutions_2_sets :: forall e. (Ord e) => Set (e, Subst e)
                                           -> Set (e, Subst e)
                                           -> Set (e, Subst e)
unifySolutions_2_sets s0 s1 = setUnions perElementSolutions
  where common = S.intersection (S.map fst s0) (S.map fst s1) :: Set e
        perElementSolutions = S.map f common :: Set (Set (e, Subst e))
          where f :: e -> Set (e, Subst e)
                f e = setUnions $ S.map u' s0'
                  where u' :: (e, Subst e) -> Set (e, Subst e)
                        u' = flip unifySolutions_1_to_many s1'
                        s0' = S.filter ((==) e . fst) s0
                        s1' = S.filter ((==) e . fst) s1

unifySolutions_1_to_many :: forall e. (Ord e) =>     (e, Subst e)
                                              -> Set (e, Subst e)
                                              -> Set (e, Subst e)
unifySolutions_1_to_many es0@(e0,_) ess1 =
  S.map fromJust $ S.filter isJust $ S.map (unifySolutions_2 es0) ess1'
  where ess1' = S.filter ((==) e0 . fst) ess1 :: Set (e, Subst e)

unifySolutions_2 :: forall e. (Ord e) =>       (e, Subst e)
                                      ->       (e, Subst e)
                                      -> Maybe (e, Subst e)
unifySolutions_2 (e,s) (e',s')
  |  e == e' = case unifySubsts_2 s s' of Nothing  -> Nothing
                                          Just sub -> Just (e, sub)
  | otherwise = Nothing


-- | = unifying substitutions

unifySubsts :: forall e. Eq e
            => [Subst e] -> Maybe (Subst e)
unifySubsts = foldl f (Just M.empty) where
  f :: Maybe (Subst e) -> Subst e -> Maybe (Subst e)
  f Nothing _ = Nothing
  f (Just s1) s2 = unifySubsts_2 s1 s2

unifySubsts_2 :: forall e. Eq e
               => Subst e -> Subst e -> Maybe (Subst e)
unifySubsts_2 m n = case fromM of Nothing  -> Nothing
                                  Just sub -> Just $ M.union sub fromN
 -- `fromM` = the result of unifySubstsing each elt of `m` with `n`.
   -- If that unification fails, we never need to find `fromN`.
 -- `fromN` = what is left in `n` that matched with nothing in `m`.
  where fromN = S.foldl f M.empty $ S.difference (M.keysSet n) (M.keysSet m)
          where f :: Subst e -> Var -> Subst e
                f sub v = M.insert v ((M.!) n v) sub
        fromM = M.foldlWithKey f (Just M.empty) m
          where f :: Maybe (Subst e) -> Var -> e
                  -> Maybe (Subst e)
                f eSub v a = case eSub of
                  Nothing  -> eSub
                  Just sub -> case M.lookup v n of
                    Just a' | a == a' -> Just $ M.insert v a sub
                    Nothing           -> Just $ M.insert v a sub
                    _                 -> Nothing
