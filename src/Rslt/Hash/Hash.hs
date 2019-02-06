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

subExprs :: Rslt -> [[Role]] -> Addr -> Either String (Set Addr)
subExprs r rls a =
  S.fromList <$> ifLefts "subExprs" its
  where its :: [Either String Addr]
        its = map (subExpr r a) rls

subExpr :: Rslt -> Addr -> [Role] -> Either String Addr
subExpr _ a [] = Right a
subExpr r a (rl : rls) = do
  (aHas :: Map Role Addr) <-
    prefixLeft ("subExpr, looking up Addr" ++ show a)
    $ has r a
  (member_of_a :: Addr) <-
    maybe (Left $ "subExpr, looking up Role " ++ show rl ++ ".") Right
    $ M.lookup rl aHas
  subExpr r member_of_a rls


hLookup :: Rslt -> HExpr -> Either String (Set Addr)

hLookup r (HMap m) = do
  let found :: Map Role (Either String (Set Addr))
      found = M.map (hLookup r) m
  (found :: Map Role (Set Addr)) <-
    ifLefts_map "hLookup called on HMap calculating found" found

  let roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
      roleHostCandidates role as = do
        -- The `as` are presumed to fill the role `role` in some host.
        -- This returns all those hosts.
        (roleHostPairs :: Set (Role, Addr)) <-
          S.unions <$>
          ( ifLefts_set "hLookup on HMap / f"
            $ S.map (isIn r) as )
        Right $ S.map snd
          $ S.filter ((==) role . fst) roleHostPairs

  (hosts :: Map Role (Set Addr)) <-
    ifLefts_map "hLookup called on HMap calculating hosts"
    $ M.mapWithKey roleHostCandidates found
  case null hosts of
    True -> Right S.empty
    False -> Right $ foldl1 S.intersection $ M.elems hosts

hLookup r (HEval hm paths) = do
  (hosts :: Set Addr) <-
    hLookup r $ HMap hm
  (its :: Set (Set Addr)) <-
    ( ifLefts_set "hLookup called on HEval, mapping over hosts"
      $ S.map (subExprs r paths) hosts )
  Right $ S.unions its

-- | TRICK: For speed, put the most selective searches first in the list.
hLookup r (HAnd hs) = foldr1 S.intersection <$>
                    ( ifLefts "hLookup called on HAnd" $ map (hLookup r) hs )

hLookup r (HOr hs) = foldr1 S.union <$>
                   ( ifLefts "hLookup called on HOr" $ map (hLookup r) hs )

hLookup r (HDiff base exclude) = do
  b <- prefixLeft "hLookup called on HDiff calculating base"
       $ hLookup r base
  e <- prefixLeft "hLookup called on HDiff calculating exclude"
       $ hLookup r exclude
  Right $ S.difference b e

hLookup r (HExpr e) = S.singleton <$> lookup r e
