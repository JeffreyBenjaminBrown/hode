{-# LANGUAGE
ScopedTypeVariables,
TupleSections #-}

module Hode.Rslt.Sort where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Control.Monad (mapM,foldM)


import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.Edit.Terminal (delete)
import Hode.Rslt.Index
import Hode.Rslt.RLookup
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | `allRelsInvolvingTplts r ts` finds every `Rel`
-- that uses a member of `ts` as a `Tplt`.
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

-- | `allNormalMembers r rs` finds every non-`Tplt`
-- member of anything in `rs`.
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
     [Addr]     -- ^ the `Expr`s to sort
  -> [TpltAddr] -- ^ how to sort
  -> Rslt       -- ^ the original `Rslt`
  -> Either String Rslt -- ^ the `Expr`s, every `Tplt` in the `BinTpltOrder`,
  -- every `Rel` involving those `Tplt`s, and every member of those `Rel`s
restrictRsltForSort es ts r =
  prefixLeft "restrictRsltForSort: " $ do
  rels :: Set RelAddr  <- allRelsInvolvingTplts r ts
  mems :: [MemberAddr] <- allNormalMembers r $ S.toList rels
  let refExprs = M.restrictKeys (_addrToRefExpr r) $
                 S.unions [ S.fromList $ es ++ ts ++ mems,
                            rels ]
  Right $ mkRslt refExprs

-- | `isTop r (orient,t) a` tests whether,
-- with respect to `t` under the orientation `ort`,
-- no `Expr` in `r` is greater than the one at `a`.
-- For instance, if `ort` is `LeftIsBigger`,
-- and `a` is on the right side of some relationship using
-- `t` as its `Tplt`, then the result is `False`.
isTop :: Rslt -> (BinOrientation, TpltAddr) -> Addr
        -> Either String Bool
isTop r (ort,t) a =
  prefixLeft "isTop: " $ do
  let roleIfLesser = case ort of
        LeftIsBigger  -> RoleMember 2
        RightIsBigger -> RoleMember 1
  relsInWhichItIsLesser <- hExprToAddrs r mempty $
    HMap $ M.fromList [ (RoleTplt,     HExpr $ Addr t),
                        (roleIfLesser, HExpr $ Addr a) ]
  Right $ null relsInWhichItIsLesser

allTops :: Rslt
        -> (BinOrientation, TpltAddr)
        -> [Addr] -- ^ candidates
        -> Either String [Addr]
allTops r (bo,t) as =
  prefixLeft "allTops" $ do
  let withIsTop :: Addr -> Either String (Bool, Addr)
      withIsTop a = (,a) <$> isTop r (bo,t) a
  map snd . filter fst <$> mapM withIsTop as

justUnders :: (BinOrientation, TpltAddr) -> Rslt -> Addr
           -> Either String (Set Addr)
justUnders (bo,t) r a = let
  h :: HExpr = let
    (bigger, smaller) = case bo of
      LeftIsBigger ->  (1,2)
      RightIsBigger -> (2,1)
    rel = HMap $ M.fromList
      [ ( RoleTplt,          HExpr $ Addr t),
        ( RoleMember bigger, HExpr $ Addr a ) ]
    in HEval rel [[RoleMember smaller]]
  in prefixLeft "justUnders" $
     hExprToAddrs r mempty h

-- | `deleteHostsThenDelete t a r` removes from `r` every
-- rel in which `a` is a member and `t` is the template,
-- and then removes `a`.
deleteHostsThenDelete ::
  TpltAddr -> Addr -> Rslt -> Either String Rslt
deleteHostsThenDelete t a r =
  prefixLeft "deleteHostsThenDelete" $ do
  hosts :: Set Addr <-
    S.map snd <$> isIn r a
  r1 <- foldM (flip delete) r hosts
  delete a r1

data Kahn = Kahn { kahnRslt   :: Rslt
                 , kahnTops   :: [Addr]
                 , kahnSorted :: [Addr] }

kahnIterate :: (BinOrientation, TpltAddr) -> Kahn
            -> Either String Kahn
kahnIterate (bo,t) (Kahn r (top:tops) acc) =
  prefixLeft "kahnIterate" $ do
  jus :: Set Addr <- justUnders (bo,t) r top
  error ""
