{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HLookup (
    hFind        -- HExpr -> Find Addr Rslt
  , hExprToExpr  -- Rslt -> HExpr               -> Either String Expr
  , hExprToAddrs -- Rslt -> Subst Addr -> HExpr -> Either String (Set Addr)
  ) where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hash.HTypes
import Hash.HUtil
import Qseq.QTypes
import Rslt.RTypes
import Rslt.RLookup
import Util.Misc


hFind :: HExpr -> Find Addr Rslt
hFind he = Find f $ hVars he
  where f rslt subst = hExprToAddrs rslt subst he


-- | `hExprToExpr` is useful for parsing a not-yet-extant `Expr`.

hExprToExpr :: Rslt -> HExpr -> Either String Expr
hExprToExpr _ (HExpr e) = Right e

hExprToExpr r h@(HMap mh) =
  prefixLeft "-> hExprToExpr, called on HMap" $ do
    (me :: Map Role Expr) <- ifLefts_map
                             $ M.map (hExprToExpr r) mh
    (t :: Expr) <-
      maybe (Left $ "hExprToExpr: no Tplt in " ++ show h)
      Right $ M.lookup RoleTplt me
    case t of ExprTplt _ -> Right ()
              x -> Left $ "hExprToExpr: in " ++ show h
                   ++ ", the expression " ++ show x ++ " is not a Tplt."
    ta <- arity r t
    if M.size (M.delete RoleTplt me) == ta then Right ()
      else Left $ "hExprToExpr: arity mismatch between "
           ++ show h ++ " and its Tplt " ++ show t
    Right $ ExprRel $ Rel (M.elems $ M.delete RoleTplt me) t
      -- PITFALL: This M.elems clause relies on the fact that those
      -- elems will be ordered from RoleMember 1 to RoleMember n.
      -- That's true for the same reason this is true:
      --     > M.elems $ M.fromList [(1,"z"),(2,"a")]
      --     ["z","a"]

hExprToExpr _ h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr and HMap constructors can be so converted."


-- | `hExprToAddrs` is the Hash search workhorse.

hExprToAddrs :: Rslt -> Subst Addr -> HExpr -> Either String (Set Addr)

hExprToAddrs r s (HMap m) =
  prefixLeft "-> hExprToAddrs called on HMap" $ do
    (found :: Map Role (Set Addr)) <-
      prefixLeft ", calculating found"
      $ ifLefts_map $ M.map (hExprToAddrs r s) m

    let roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
        roleHostCandidates role as = do
          -- The `as` are presumed to fill the role `role` in some host.
          -- This returns all those hosts.
          (roleHostPairs :: Set (Role, Addr)) <-
            prefixLeft ",on HMap / f" $ S.unions
             <$> ifLefts_set (S.map (isIn r) as )
          Right $ S.map snd
            $ S.filter ((==) role . fst) roleHostPairs

    (hosts :: Map Role (Set Addr)) <-
      prefixLeft ", calculating hosts"
      $ ifLefts_map $ M.mapWithKey roleHostCandidates found
    case null hosts of
      True -> Right S.empty
      False -> Right $ foldl1 S.intersection $ M.elems hosts

hExprToAddrs r s (HEval hm paths) =
  prefixLeft "-> hExprToAddrs called on HEval" $ do
    (hosts :: Set Addr)     <-
      prefixLeft ", mapping over hosts"
      $ hExprToAddrs r s hm
    (its :: Set (Set Addr)) <-
      ifLefts_set $ S.map (subExprs r paths) hosts
    Right $ S.unions its

hExprToAddrs _ s (HVar v) =
  maybe (Left $ keyErr "hExprToAddrs" v s) (Right . S.singleton)
  $ M.lookup v s

hExprToAddrs r _ (HExpr e) = S.singleton <$> exprToAddr r e

hExprToAddrs r s (HDiff base exclude) =
  prefixLeft "-> hExprToAddrs called on HDiff" $ do
    b <- prefixLeft ", calculating base"
         $ hExprToAddrs r s base
    e <- prefixLeft ", calculating exclude"
         $ hExprToAddrs r s exclude
    Right $ S.difference b e

-- | TRICK: For speed, put the most selective searches first in the list.
hExprToAddrs r s (HAnd hs) =
  prefixLeft "-> hExprToAddrs called on HAnd"
  $ foldr1 S.intersection
  <$> ifLefts (map (hExprToAddrs r s) hs )

hExprToAddrs r s (HOr hs) =
  prefixLeft "-> hExprToAddrs called on HOr"
  $ foldr1 S.union
  <$> ifLefts (map (hExprToAddrs r s) hs )
