{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Hash.Hash where

import           Prelude hiding (lookup)
import           Data.Either
import qualified Data.List as L
import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.RTypes
import Rslt.Lookup
import Rslt.Hash.HTypes
import Util


hExprToExpr :: HExpr -> Either String Expr
hExprToExpr (HExpr e) = Right e
hExprToExpr h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr constructor can be converted to an Expr.\n"

retrieveIts :: Rslt -> [[Role]] -> Addr -> Either String (Set Addr)
retrieveIts r rls a =
  S.fromList <$> ifLefts "retrieveIts" its
  where its :: [Either String Addr]
        its = map (retrieveIts1 r a) rls

retrieveIts1 :: Rslt -> Addr -> [Role] -> Either String Addr
retrieveIts1 _ a [] = Right a
retrieveIts1 r a (rl : rls) = do
  (aHas :: Map Role Addr) <-
    prefixLeft ("retrieveIts1, looking up Addr" ++ show a)
    $ has r a
  (member_of_a :: Addr) <-
    maybe (Left $ "retrieveIts1, looking up Role " ++ show rl ++ ".") Right
    $ M.lookup rl aHas
  retrieveIts1 r member_of_a rls


hFind :: Rslt -> HExpr -> Either String (Set Addr)

hFind r (HMap m) = do
  let found :: Map Role (Either String (Set Addr))
      found = M.map (hFind r) m
  (found :: Map Role (Set Addr)) <-
    ifLefts_map "hFind called on HMap calculating found" found

  let roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
      roleHostCandidates role as = do
        -- The `as` are presumed to fill the role `role` in some host.
        -- This returns all those hosts.
        (roleHostPairs :: Set (Role, Addr)) <-
          S.unions <$>
          ( ifLefts_set "hFind on HMap / f"
            $ S.map (isIn r) as )
        Right $ S.map snd
          $ S.filter ((==) role . fst) roleHostPairs

  (hosts :: Map Role (Set Addr)) <-
    ifLefts_map "hFind called on HMap calculating hosts"
    $ M.mapWithKey roleHostCandidates found
  case null hosts of
    True -> Right S.empty
    False -> Right $ foldl1 S.intersection $ M.elems hosts

hFind r (HEval hm paths) = do
  (hosts :: Set Addr) <-
    hFind r $ HMap hm
  (its :: Set (Set Addr)) <-
    ( ifLefts_set "hFind called on HEval, mapping over hosts"
      $ S.map (retrieveIts r paths) hosts )
  Right $ S.unions its

-- | TRICK: For speed, put the most selective searches first in the list.
hFind r (HAnd hs) = foldr1 S.intersection <$>
                    ( ifLefts "hFind called on HAnd" $ map (hFind r) hs )

hFind r (HOr hs) = foldr1 S.union <$>
                   ( ifLefts "hFind called on HOr" $ map (hFind r) hs )

hFind r (HDiff base exclude) = do
  b <- prefixLeft "hFind called on HDiff calculating base"
       $ hFind r base
  e <- prefixLeft "hFind called on HDiff calculating exclude"
       $ hFind r exclude
  Right $ S.difference b e

hFind r (HExpr e) = S.singleton <$> lookup r e
