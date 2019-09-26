{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Sort where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.Index
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | Synonyms.
data BinOrientation = LeftIsBigger | RightIsBigger

type RelAddr    = Addr
type MemberAddr = Addr
type TpltAddr   = Addr

type BinTpltOrder = Map Int (BinOrientation, TpltAddr)

-- | A `TopSets` is only meaningful in the context of a `BinTpltOrder`.
-- The first member of each pair is a number of Tplts in the BinTpltOrder.
-- Initially the only member, (0,_), represents the entire graph.
-- Whenever a new pair is pushed onto a `TopSets`,
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
type TopSets = [(Int,[Addr])]


allRelsInvolvingTplts ::
  Rslt -> [TpltAddr] -> Either String (Set RelAddr)
allRelsInvolvingTplts r ts =
  prefixLeft "allRelsInvolvingTplts: " $ do
  hostRels :: [Set (Role, RelAddr)] <-
    ifLefts $ map (isIn r) ts
  Right $ S.unions $
        map ( S.map snd .
              S.filter ((==) RoleTplt . fst) )
        hostRels

allNormalMembers ::
  Rslt -> [RelAddr] -> Either String [RelAddr]
allNormalMembers r rels =
  prefixLeft "allNormalMembers: " $ do
  members :: [Map Role Addr] <-
    ifLefts $ map (has r) rels
  Right $ concatMap
    ( M.elems .
      flip M.withoutKeys (S.singleton RoleTplt) )
    members

restrictRsltForSort ::
     [Addr] -- ^ the `Expr`s to sort
  -> BinTpltOrder
  -> Rslt -- ^ the original `Rslt`
  -> Either String Rslt -- ^ the `Expr`s, every `Tplt` in the `BinTpltOrder`,
  -- every `Rel` involving those `Tplt`s, and every member of those `Rel`s
restrictRsltForSort es bto r =
  prefixLeft "restrictRsltForSort: " $ do
  let ts :: [TpltAddr] =  map snd $ M.elems bto
  rels :: Set RelAddr  <- allRelsInvolvingTplts r ts
  mems :: [MemberAddr] <- allNormalMembers r $ S.toList rels
  let refExprs = M.restrictKeys (_addrToRefExpr r) $
                 S.unions [ S.fromList $ es ++ ts ++ mems,
                            rels ]
  Right $ mkRslt refExprs

-- | `maximal r (orient,t) k a` tests whether,
-- with respect to `t` under the orientation `orient`,
-- no `Expr` in `r` is greater than the one at `a`.
-- For instance, if `orient` is `LeftIsBigger`,
-- and `a` is on the right side of some relationship using
-- `t` as its `Tplt`, then the result is `False`.
maximal :: Rslt -> (BinOrientation, TpltAddr) -> Addr
                 -> Either String Bool
maximal r (orient,t) a =
  prefixLeft "maximal: " $ do
  let roleIfLesser = case orient of
        LeftIsBigger -> RoleMember 2
        RightIsBigger -> RoleMember 1
  relsInWhichItIsLesser <- hExprToAddrs r mempty $
    HMap $ M.fromList [ (RoleTplt,     HExpr $ Addr t),
                        (roleIfLesser, HExpr $ Addr a) ]
  Right $ null relsInWhichItIsLesser

