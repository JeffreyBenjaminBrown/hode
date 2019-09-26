{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Sort where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Rslt.RTypes
import Hode.Rslt.RLookup
import Hode.Util.Misc


-- | Synonyms.
type RelSyn    = Addr
type MemberSyn = Addr
type TpltSyn   = Addr

type BinTpltOrder = Map Int TpltSyn

-- | A `NestedMaxes` is only meaningful in the context of a `BinTpltOrder`.
-- The first member of each pair is a number of Tplts in the BinTpltOrder.
-- Initially the only member, (0,_), represents the entire graph.
-- Whenever a new pair is pushed onto a `NestedMaxes`,
-- its addresses will be taken (not copied) from the previous head
-- it will start with a number higher than the previous head,
-- which indicates the number of `Tplt`s in the `BinTpltOrder`
-- that they are all "equally maximal" w/r/t.
-- (The order is partial, so they're not exactly equal,
-- but none is bigger than the others.)
-- The `fst`s do not have to increase consecutively.
-- For instance, if the only element of the list is `(0,as0)`,
-- and nothing in `as0` is involved in a relationship
-- that uses the first or second `Tplt` in the `BinTpltOrder`,
-- then the next pair to be pushed onto the front of the list
-- will have a `fst` greater than `2`.
type NestedMaxes = [(Int,[Addr])]


allRelsInvolvingTplts ::
  Rslt -> BinTpltOrder -> Either String (Set RelSyn)
allRelsInvolvingTplts r bto = do
  let ts :: [TpltSyn] = M.elems bto
  hostRels :: [Set (Role,RelSyn)] <-
    ifLefts $ map (isIn r) ts
  Right $ S.unions $
        map ( S.map snd .
              S.filter ((==) RoleTplt . fst) )
        hostRels

