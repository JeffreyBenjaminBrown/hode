{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Hash.Lookup (
    hFind              -- ^ HExpr -> Find Addr Rslt
  , usesTransitiveTplt -- ^ Rslt          -> Addr -> Either String Bool
  , hMatches           -- ^ Rslt -> HExpr -> Addr -> Either String Bool
  , hExprToExpr        -- ^ Rslt -> HExpr         -> Either String Expr
  , module X
  ) where

import           Data.Either
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import           Hode.Hash.MkHExpr
import           Hode.Hash.Types
import           Hode.Hash.Util
import           Hode.Hash.Lookup.Transitive
import qualified Hode.Hash.Lookup.Transitive as X
import           Hode.Qseq.Types
import           Hode.Rslt.Types
import           Hode.Rslt.Lookup
import           Hode.Util.Misc


hFind :: HExpr -> Find Addr Rslt
hFind he = Find f $ hVars he
  where f rslt subst = hExprToAddrs rslt subst he

usesTransitiveTplt :: Rslt -> Addr -> Either String Bool
usesTransitiveTplt r a =
  prefixLeft "usesTransitiveTplt:" $ do
  case addrToRefExpr r a of
    Right (Rel' (Rel _ t)) -> do
      isTransitive :: HExpr <-
        pRelToHExpr r ( Closed [ PNonRel Any
                              , PNonRel $ PExpr $ Phrase "transitive"]
                        ["is"] )
      hMatches r (HEval isTransitive $ [[RoleMember 1]]) t
    Right _ -> Right False
    Left s -> Left s

-- | The idea of `hMatches` is to determine whether an `HExpr`
-- matches the `Expr` at an `Addr`, without having to find everything
-- else that matches the `HExpr`. It isn't totally implemented.
hMatches :: Rslt -> HExpr -> Addr -> Either String Bool
hMatches r h0 a0 =
  prefixLeft ("hMatches, called on " ++ show (h0,a0) ++ ":") $ do
  e0 :: Expr <- addrToExpr r a0
  case h0 of
    HExpr e -> unAddr r e >>= Right . (== e0)

    HMap (mh :: Map Role HExpr) -> do
      ma :: Map Role Addr <- ifLefts_map $
        M.mapWithKey (\rol _ -> hasInRole r rol a0) mh
      let mPairs :: Map Role (HExpr,Addr) =
            M.mapWithKey f mh where
            f rol h = (h,a) where
              a :: Addr = ma M.! rol
      mCompares :: Map Role Bool <- ifLefts_map $
        M.map (uncurry $ hMatches r) mPairs
      Right $ and mCompares

    (HEval _ _) -> do -- TODO ? speed: Arguably, this branch cheats.
    -- It finds everything that `h0` matches, and then sees if  `a0`
    -- is in that set. But to search up from a0 instead of down from h0
    --  would still be a tree search, so it's not clear which is better.
    -- (If `a0` is a member of many things, better to search from `h0`.)
      hImages :: Set Addr <- hExprToAddrs r mempty h0
      Right $ S.member a0 hImages

    HVar _ -> Left "HVar: not implemented."

    HDiff beIn beNotIn -> do
      bi :: Bool <- hMatches r beIn a0
      bni :: Bool <- hMatches r beNotIn a0
      Right $ bi && not bni

    HAnd hs -> do
      bs :: [Bool] <- ifLefts $ map (\h -> hMatches r h a0) hs
      Right $ and bs

    HOr hs ->
      Right $ or $ rights $ map (\h -> hMatches r h a0) hs

    HTplts -> Right $ case e0 of
      ExprTplt _ -> True
      _          -> False

    HReach _ _ _ -> error "not implemented: Hash.Lookup.hMatches, called on an HReach"
    HTrans _ _ _ _ _ -> error "not implemented: Hash.Lookup.hMatches, called on an HTrans"
    HMemberHosts _ -> error "not implemented: Hash.Lookup.hMatches, called on an HMemberHosts"
    HMemberHostsRec _ _ -> error "not implemented: Hash.Lookup.hMatches, called on an HMemberHostsRec."

-- | `hExprToExpr` is useful for parsing a not-yet-extant `Expr`.
hExprToExpr :: Rslt -> HExpr -> Either String Expr
hExprToExpr _ (HExpr e) = Right e

hExprToExpr r h@(HMap mh) =
  prefixLeft "hExprToExpr, called on HMap:" $ do
  me :: Map Role Expr <-
    ifLefts_map $ M.map (hExprToExpr r) mh
  t :: Expr <-
    maybe (Left $ "No Tplt in " ++ show h) Right $
    M.lookup (RoleInRel' RoleTplt) me
  case t of
    ExprTplt _ -> Right ()
    x -> Left $ "hExprToExpr: in " ++ show h
      ++ ", the expression " ++ show x ++ " is not a Tplt."
  ta <- arityIn r t

  if M.size (M.delete (RoleInRel' RoleTplt) me) == ta
    then Right ()
    else Left $ "hExprToExpr: arity mismatch between "
         ++ show h ++ " and its Tplt " ++ show t
  Right $ ExprRel $ Rel (M.elems $ M.delete (RoleInRel' RoleTplt) me) t
    -- PITFALL: This M.elems clause relies on the fact that those
    -- elems will be ordered from RoleMember 1 to RoleMember n.
    -- That's true for the same reason this is true:
    --     > M.elems $ M.fromList [(1,"z"),(2,"a")]
    --     ["z","a"]

hExprToExpr _ h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr and HMap constructors can be so converted."
