{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.Index (mkRslt)
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


r :: Rslt
r = mkRslt $ M.fromList [
  (0,Phrase' ""),
  (1,Tplt' $ Tplt (Just 0) [0] Nothing),
  (2,Phrase' "a"),
  (3,Phrase' "b"),
  (4,Rel' (Rel [2,3] 1))]

h :: HExpr
h = HMap m

m :: HMap
m = M.fromList
  [ ( RoleTplt,
      HExpr $ ExprTplt $ Tplt
      (Just $ Phrase "") [Phrase ""] Nothing ),
    ( RoleMember 3,
      HExpr $ Phrase "b") ]

s :: Subst Addr
s = mempty

-- problem: hExprToAddrs r s (HMap m) is empty.
-- Below are the definitions in that function.

found :: Map Role (Set Addr)
Right found =
  ifLefts_map $ M.map (hExprToAddrs r s) m

roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
roleHostCandidates role as = do
  -- The `as` are presumed to fill the role `role` in some host.
  -- This returns all those hosts.
  (roleHostPairs :: Set (Role, Addr)) <-
    prefixLeft ",on HMap / f" $ S.unions
     <$> ifLefts_set (S.map (isIn r) as )
  Right $ S.map snd
    $ S.filter ((==) role . fst) roleHostPairs

hosts :: Map Role (Set Addr)
Right hosts =
  ifLefts_map $ M.mapWithKey roleHostCandidates found

result :: Set Addr
Right result = case null hosts of
  True -> Right S.empty
  False -> Right $ foldl1 S.intersection $ M.elems hosts
