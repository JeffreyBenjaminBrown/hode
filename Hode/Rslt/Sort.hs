-- | PITFALL: This algorithm makes sense only on a
-- graph-like subset of an Rslt -- one in which:
--   (1) all "edges" are binary `Rel`s
--   (2) edges never involve another edge
--   (3) the "edge label" (`Tplt`) `e` to sort by
--       plays no `Role` but `Tplt`
--   (4) none of the `Expr`s the user wants to sort
--       is a `Rel` with `e` as its `Tplt`
--
-- PITFALL: Some of these functions are written
-- to handle multiple `Tplt`s, but then I lost interest.

{-# LANGUAGE
ScopedTypeVariables,
TupleSections #-}

module Hode.Rslt.Sort (
    Kahn(..)
  , kahnSort -- ^  Rslt -> (BinOrientation, TpltAddr) -> [Addr]
             -- -> Either String [Addr]

   -- | = Internals
  , kahnRecurse -- ^  (BinOrientation, TpltAddr) -> Kahn
                -- -> Either String Kahn
  , kahnIterate -- ^  (BinOrientation, TpltAddr) -> Kahn
                -- -> Either String Kahn

  , allRelsInvolvingTplts -- ^  Rslt -> [TpltAddr]
                          -- -> Either String (Set RelAddr)
  , allNormalMembers      -- ^  Rslt -> [RelAddr]
                          -- -> Either String [RelAddr]
  , restrictRsltForSort
    -- ^  [Addr]             -- ^ the `Expr`s to sort
    -- -> [TpltAddr]         -- ^ how to sort
    -- -> Rslt               -- ^ the original `Rslt`
    -- -> Either String Rslt
      -- ^ the `Expr`s, every `Tplt` in the `BinTpltOrder,
      -- every `Rel` involving those `Tplt`s,
      -- and every member of those `Rel`s
  , allExprsButTpltsOrRelsUsingThem -- ^ Rslt -> [TpltAddr]
                                    -- -> Either String (Set Addr)
  , isTop -- ^ Rslt -> (BinOrientation, TpltAddr) -> Addr
          -- -> Either String Bool
  , allTops -- ^   Rslt
            -- -> (BinOrientation, TpltAddr)
            -- -> [Addr] -- ^ candidates
            -- -> Either String [Addr]
  , justUnders -- ^ (BinOrientation, TpltAddr) -> Rslt -> Addr
               -- -> Either String (Set Addr)
  , deleteHostsThenDelete -- ^ Addr -> Rslt -> Either String Rslt
  ) where

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


data Kahn = Kahn { kahnRslt   :: Rslt
                 , kahnTops   :: [Addr]
                 , kahnSorted :: [Addr] }
  deriving (Eq,Ord,Show)

-- | Depth-first search.
-- (For BFS, reverse the order of the expression
-- `newTops ++ tops` in `kahnIterate`.
kahnSort :: Rslt -> (BinOrientation, TpltAddr) -> [Addr]
         -> Either String [Addr]
kahnSort r (bo,t) as =
-- TODO speed: this calls `restrictRsltForSort` and `allRelsInvolvingTplts`, but `restrictRsltForSort` also calls `allRelsInvolvingTplts`, with the same arguments. I don't know whether GHC will optimize that away.
  prefixLeft "kahnSort:" $ do
  rels :: Set Addr <- allRelsInvolvingTplts r [t]
  r1 :: Rslt <- restrictRsltForSort as [t] r
    -- restrict to "nodes", "edges", and the "edge label" at `t`
  nodes :: [Addr] <-
    S.toList <$> allExprsButTpltsOrRelsUsingThem r1 [t]
  tops :: [Addr] <- allTops r1 (bo,t) nodes
  Kahn r2 _ res <- kahnRecurse (bo,t) $ Kahn r1 tops []
  case null $ S.intersection rels $
       S.fromList $ M.keys $ _addrToRefExpr r2 of
    True -> Right $ filter (flip elem $ S.fromList as) res
    False -> Left "data has at least one cycle."


-- | = Internals

-- | Splitting kahnRecurse from kahnIterate
-- might (I don't know) be slower,
-- but it lets me test a single iteration.
kahnRecurse :: (BinOrientation, TpltAddr) -> Kahn
            -> Either String Kahn
kahnRecurse bt k =
  prefixLeft "kahnRecurse:" $
  case kahnTops k of
    [] -> Right k
    _ -> kahnIterate bt k >>= kahnRecurse bt

kahnIterate :: (BinOrientation, TpltAddr) -> Kahn
            -> Either String Kahn
kahnIterate _ k@(Kahn _ [] _) =
  Right k
kahnIterate (bo,t) (Kahn r (top:tops) acc) =
  prefixLeft "kahnIterate:" $ do
  jus :: Set Addr <- justUnders (bo,t) r top
  r1 :: Rslt <- deleteHostsThenDelete top r
  newTops :: [Addr] <- allTops r1 (bo,t) $
                       S.toList jus
  Right $ Kahn r1 (newTops ++ tops) (top : acc)

-- | `allRelsInvolvingTplts r ts` finds every `Rel`
-- that uses a member of `ts` as a `Tplt`.
allRelsInvolvingTplts ::
  Rslt -> [TpltAddr] -> Either String (Set RelAddr)
allRelsInvolvingTplts r ts =
  prefixLeft "allRelsInvolvingTplts:" $ do
  hostRels :: [Set (Role, RelAddr)] <-
    ifLefts $ map (isIn r) ts
  Right $ S.unions $
    map ( S.map snd .
          S.filter ((==) (RoleInRel' RoleTplt) . fst) )
    hostRels

-- | `allNormalMembers r rs` finds every non-`Tplt`
-- member of anything in `rs`.
allNormalMembers ::
  Rslt -> [RelAddr] -> Either String [RelAddr]
allNormalMembers r rels =
  prefixLeft "allNormalMembers:" $ do
  members :: [Map Role Addr] <-
    ifLefts $ map (has r) rels
  Right $ concatMap
    ( M.elems . ( flip M.withoutKeys
                  (S.singleton $ RoleInRel' RoleTplt) ) )
    members

restrictRsltForSort ::
     [Addr]     -- ^ the `Expr`s to sort
  -> [TpltAddr] -- ^ how to sort
  -> Rslt       -- ^ the original `Rslt`
  -> Either String Rslt -- ^ the `Expr`s, every `Tplt` in the `BinTpltOrder`,
  -- every `Rel` involving those `Tplt`s, and every member of those `Rel`s
restrictRsltForSort es ts r =
  prefixLeft "restrictRsltForSort:" $ do
  rels :: Set RelAddr  <- allRelsInvolvingTplts r ts
  mems :: [MemberAddr] <- allNormalMembers r $ S.toList rels
  let refExprs = M.restrictKeys (_addrToRefExpr r) $
                 S.unions [ S.fromList $ es ++ ts ++ mems,
                            rels ]
  Right $ mkRslt refExprs

-- | `allExprsButTpltsOrRelsUsingThem r ts` returns the "nodes"
-- of the sub-`Rslt` (which must be of a specific form --
-- see header comment.) Specifically, it removes
-- from the `Set Addr` in `r` every `Tplt` in `ts`,
-- and every `Rel` in which some `t` in `ts` is the `Tplt`.
--
-- PITFALL: Call `restrictRsltForSort` on the `Rslt`
-- before calling this on it. Otherwise garbage like the
-- words used by the `Tplt` will be part of the sort results.
allExprsButTpltsOrRelsUsingThem ::
  Rslt -> [TpltAddr] -> Either String (Set Addr)
allExprsButTpltsOrRelsUsingThem r ts =
  prefixLeft "allExprsButTpltsOrRelsUsingThem:" $ do
  let as :: Set Addr =
        S.fromList $ M.keys $ _addrToRefExpr r
  tsUsers :: Set Addr <-
    hExprToAddrs r mempty $
    HMap $ M.singleton (RoleInRel' RoleTplt) $
    HOr $ map (HExpr . Addr) ts
  Right ( S.difference
          ( S.difference as $ S.fromList ts )
          tsUsers )

-- | `isTop r (orient,t) a` tests whether,
-- with respect to `t` under the orientation `ort`,
-- no `Expr` in `r` is greater than the one at `a`.
-- For instance, if `ort` is `RightFirst`,
-- and `a` is on the right side of some relationship using
-- `t` as its `Tplt`, then the result is `False`.
isTop :: Rslt -> (BinOrientation, TpltAddr) -> Addr
      -> Either String Bool
isTop r (ort,t) a =
  prefixLeft "isTop:" $ do
  let roleIfLesser = case ort of
        RightFirst  -> RoleInRel' $ RoleMember 2
        LeftFirst -> RoleInRel' $ RoleMember 1
  relsInWhichItIsLesser <- hExprToAddrs r mempty $
    HMap $ M.fromList [ (RoleInRel' $ RoleTplt, HExpr $ Addr t),
                        (roleIfLesser,          HExpr $ Addr a) ]
  Right $ null relsInWhichItIsLesser

isIsolated :: Rslt -> TpltAddr -> Addr
           -> Either String Bool
isIsolated r t a =
  prefixLeft "isIsolated:" $ do
  connections :: Set Addr <-
        hExprToAddrs r mempty $ HAnd
        [ HMap $ M.singleton (RoleInRel' RoleTplt) (HExpr $ Addr t)
        , HMember $ HExpr $ Addr a ]
  Right $ if null connections then True else False

allTops :: Rslt
        -> (BinOrientation, TpltAddr)
        -> [Addr] -- ^ candidates
        -> Either String [Addr]
allTops r (bo,t) as =
  prefixLeft "allTops:" $ do
  let withIsTop :: Addr -> Either String (Bool, Addr)
      withIsTop a = (,a) <$> isTop r (bo,t) a
  map snd . filter fst <$> mapM withIsTop as

allIsolated :: Rslt -> TpltAddr
            -> [Addr] -- ^ candidates
            -> Either String [Addr]
allIsolated r t as =
  prefixLeft "allIsolated:" $ do
  let withIsIsolated :: Addr -> Either String (Bool, Addr)
      withIsIsolated a = (,a) <$> isIsolated r t a
  map snd . filter fst <$> mapM withIsIsolated as

-- | `justUnders (bo,t) r a` returns the `Addr`s that
-- lie just beneath `a`, where the menaing of "beneath"
-- depends on `bo` and `t`.
justUnders :: (BinOrientation, TpltAddr) -> Rslt -> Addr
           -> Either String (Set Addr)
justUnders (bo,t) r a = let
  h :: HExpr = let
    (bigger, smaller) = case bo of
      RightFirst ->  (1,2)
      LeftFirst -> (2,1)
    rel = HMap $ M.fromList
      [ ( RoleInRel' RoleTplt,          HExpr $ Addr t),
        ( RoleInRel' $ RoleMember bigger, HExpr $ Addr a ) ]
    in HEval rel [[RoleMember smaller]]
  in prefixLeft "justUnders:" $
     hExprToAddrs r mempty h

-- | `deleteHostsThenDelete t a r` removes from `r` every
-- rel in which `a` is a member, and then removes `a`.
deleteHostsThenDelete ::
  Addr -> Rslt -> Either String Rslt
deleteHostsThenDelete a r =
  prefixLeft "deleteHostsThenDelete:" $ do
  hosts :: Set Addr <-
    S.map snd <$> isIn r a
  r1 <- foldM (flip delete) r hosts
  delete a r1
