module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


-- | If they assign different values to the same variable, it's Nothing.
-- Otherwise it's their union.
reconcile2 :: Subst -> Subst -> Maybe Subst
reconcile2 s t = S.foldl f (Just M.empty) allKeys where
  allKeys = S.union (M.keysSet s) (M.keysSet t) :: Set Var
  f :: Maybe Subst -> Var -> Maybe Subst
  f Nothing _ = Nothing -- short-circuit (roughly)
  f (Just acc) v =
    if        S.member v (M.keysSet s)
    then if   S.member v (M.keysSet t)
         then if (M.!) t v /= (M.!) s v
              then Nothing
              else Just $ M.insert v ((M.!) s v) acc
         else      Just $ M.insert v ((M.!) s v) acc
    else           Just $ M.insert v ((M.!) t v) acc

reconcile1toMany :: Subst -> Set Subst -> Set Subst
reconcile1toMany s ss = S.map fromJust $ S.filter isJust
                $ S.map (reconcile2 s) ss

reconcile2sets :: Set Subst -> Set Subst -> Set Subst
reconcile2sets ss1 ss2 = S.unions $ S.map (\s -> reconcile1toMany s ss2) ss1

reconcile :: Set (Set Subst) -> Set Subst
reconcile ss = S.foldl reconcile2sets min rest where
  (min, rest) = S.deleteFindMin ss
