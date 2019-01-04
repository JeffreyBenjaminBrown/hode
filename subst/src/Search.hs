{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Search where

import           Data.Maybe
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Types
import Subst
import Util

  
--nqTest' :: (Ord e, Space e sp)
--          => sp -> NQuery e sp -> e -> Either Pending Bool
--nqTest' sp (QNot q) e = let sc = search' sp q
--  in case sc of
--    Solved sols -> Right $ not $ S.member e $ S.map fst sols
--    SolvedAnd sols conds -> case S.member e $ S.map fst sols of
--      True -> Right False
--      False -> Left Pending
--    SolvedOr sols conds -> Left Pending
--nqTest' sp (QCond c) e
--  | spElem e sp && c sp e = Right True
--  | otherwise             = Right False

search' :: forall e sp. (Ord e, Space e sp)
       => sp -> Query e sp -> Search' e sp
search' sp (QP pq) = Solved $ S.map (, M.empty) $ pqSearch sp pq
search' _ q@(QN _) = SolvedOr S.empty q
search' _ q@(QV _) = SolvedOr S.empty q

--search' sp (QOr qs) = Search' { solved = s, unsolved = u } where
--  search'es = map (search' sp) qs
--  s = S.unions $ map solved search'es
--  u = case map fromJust $ filter isJust $ map unsolved search'es of
--    []  -> Nothing
--    [x] -> Just x
--    x   -> Just $ QOr x


search :: forall e sp. (Ord e, Space e sp)
       => sp -> Query e sp -> Search e sp
search sp (QP pq) = Search { solved = S.map (, M.empty) $ pqSearch sp pq
                           , unsolved = Nothing }
search _ q@(QN _) = Search { solved = S.empty
                           , unsolved = Just q }
search _ q@(QV _) = Search { solved = S.empty
                           , unsolved = Just q }

search sp (QOr qs) = Search { solved = s, unsolved = u } where
  searches = map (search sp) qs
  s = S.unions $ map solved searches
  u = case map fromJust $ filter isJust $ map unsolved searches of
    []  -> Nothing
    [x] -> Just x
    x   -> Just $ QOr x

--search sp (QAnd qs) = search sp (QV $ Var "a") where -- TODO >>> unfake
--  searches = map (search sp) qs
--  (notPs, ps) = L.partition (S.null . solved) searches

testSolution :: (Ord e, Space e sp)
             => sp -> (e, Subst e) -> Query e sp -> Set (e, Subst e)
testSolution _ (e, sub) (QV v)
  | isNothing $ M.lookup v sub = S.singleton (e, M.insert v e sub)
  | Just e   == M.lookup v sub = S.singleton (e,              sub)
  | otherwise                  = S.empty
testSolution sp (e, sub) (QP q)
  | e `elem` pqSearch sp q = S.singleton (e, sub)
  | otherwise              = S.empty
testSolution sp (e, sub) (QN q) = case nqTest sp q e of
  Right True   -> S.singleton (e, sub)
  Right False  -> S.empty
  Left Pending -> S.empty -- TODO ? Ugly hack.
testSolution sp sol (QOr qs) = S.unions $ map (testSolution sp sol) qs
testSolution sp sol (QAnd []) = S.singleton sol
testSolution sp sol (QAnd (q:qs)) = 
  setUnions $ S.map testRemainingConditions $ testSolution sp sol q
  where testRemainingConditions sol = testSolution sp sol $ QAnd qs

pqSearch :: Space e sp => sp -> PQuery e sp -> Set e
pqSearch sp (QElt e) = if spElem e sp
                      then S.singleton e else S.empty
pqSearch sp (QFind f) = f sp

nqTest :: (Ord e, Space e sp)
          => sp -> NQuery e sp -> e -> Either Pending Bool
nqTest sp (QNot q) e = let sc = search sp q
  in case unsolved sc of
    Nothing -> Right $ not $ S.member e $ S.map fst $ solved sc
    Just _ -> Left Pending
nqTest sp (QCond c) e
  | spElem e sp && c sp e = Right True
  | otherwise             = Right False
