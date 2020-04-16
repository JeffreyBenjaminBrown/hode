module Hode.Rslt.Edit (
    deleteIfUnused        -- ^ Addr
                          -- -> Rslt -> Either String Rslt
  , exprToAddrInsert      -- ^ Rslt -> Expr
                          -- -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr]
                          -- -> Either String (Rslt, [Addr])
  , insert        -- ^ RefExpr
                  -- -> Rslt -> Either String Rslt
  , insertChain   -- ^ (BinOrientation, TpltAddr) -> [Addr]
                  -- -> Rslt -> Either String Rslt
  , moveRefExpr   -- ^ Addr -> Addr
                  --  -> Rslt-> Either String Rslt
  , replaceExpr   -- ^ Expr -> Addr
                  --  -> Rslt-> Either String (Rslt, Addr)
  , replaceInRole -- ^ Role -> Addr -> Addr
                  -- -> Rslt -> Either String Rslt

  , separateSimply         -- ^ TpltAddr -> [Addr] -> [Addr] -> Rslt -> Either String Rslt
  , separateSimplyMutually -- ^ TpltAddr -> [Addr]           -> Rslt -> Either String Rslt
  ) where

import           Control.Monad
import qualified Data.Map as M
import           Data.Set (Set)

import Hode.Hash.Lookup
import Hode.Hash.Types
import Hode.Rslt.Types
import Hode.Util.Misc

import Hode.Rslt.Edit.Terminal  -- only to re-export some definitions
import Hode.Rslt.Edit.AndSearch -- only to re-export some definitions
import Hode.Rslt.Edit.Replace   -- only to re-export some definitions
import Hode.Rslt.Edit.Initial   -- only to re-export some definitions


-- | `separateSimply t as bs r` deletes all `t`-relationships
-- connecting `as` and `bs` (in either diirection).
separateSimply :: TpltAddr -> [Addr] -> [Addr] -> Rslt -> Either String Rslt
separateSimply    t           as        bs        _r =
  prefixLeft "separateSimply: " $ do
  let (hTo, hFrom) =
        ( HMap $ M.fromList [ (rt, ht), (rm 1, hor as), (rm 2, hor bs) ]
        , HMap $ M.fromList [ (rt, ht), (rm 1, hor bs), (rm 2, hor as) ] )
        where
          rt  :: Role            = RoleInRel' RoleTplt
          rm  :: Int -> Role     = RoleInRel' . RoleMember
          ht  :: HExpr           =           (HExpr . ExprAddr) t
          hor :: [Addr] -> HExpr = HOr . map (HExpr . ExprAddr)
  toRels  :: Set Addr <- hExprToAddrs _r mempty hTo
  froRels :: Set Addr <- hExprToAddrs _r mempty hFrom
  _r <- foldM (flip deleteIfUnused) _r  toRels
  id $  foldM (flip deleteIfUnused) _r froRels

-- | `separateSimplyMutually t as r` deletes all `t`-relationships
-- connecting the `as`.
separateSimplyMutually :: TpltAddr -> [Addr] -> Rslt -> Either String Rslt
separateSimplyMutually    t           as        _r =
  prefixLeft "separateSimply: " $ do
  let h = HMap $ M.fromList [ (rt, ht)
                            , (rm 1, hor as)
                            , (rm 2, hor as) ]
        where
          rt  :: Role            = RoleInRel' RoleTplt
          rm  :: Int -> Role     = RoleInRel' . RoleMember
          ht  :: HExpr           =           (HExpr . ExprAddr) t
          hor :: [Addr] -> HExpr = HOr . map (HExpr . ExprAddr)
  rels  :: Set Addr <- hExprToAddrs _r mempty h
  foldM (flip deleteIfUnused) _r rels
