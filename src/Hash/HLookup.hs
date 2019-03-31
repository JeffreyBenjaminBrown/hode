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

hExprToExpr r h@(HMap mh) = do
  (me :: Map Role Expr) <- ifLefts_map "hExprToExpr"
    $ M.map (hExprToExpr r) mh
  (t :: Expr) <-
    maybe (Left $ "hExprToExpr: no Tplt in " ++ show h)
    Right $ M.lookup RoleTplt me
  case t of ExprTplt _ -> Right ()
            x -> Left $ "hExprToExpr: in " ++ show h
                 ++ ", the expression " ++ show x ++ " is not a Tplt."
  ta <- prefixLeft "hExprToExpr" $ arity r t
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

hExprToAddrs r s (HMap m) = do
  (found :: Map Role (Set Addr)) <-
    ifLefts_map "hExprToAddrs called on HMap calculating found"
    $ M.map (hExprToAddrs r s) m

  let roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
      roleHostCandidates role as = do
        -- The `as` are presumed to fill the role `role` in some host.
        -- This returns all those hosts.
        (roleHostPairs :: Set (Role, Addr)) <-
          S.unions <$>
          ( ifLefts_set "hExprToAddrs on HMap / f"
            $ S.map (isIn r) as )
        Right $ S.map snd
          $ S.filter ((==) role . fst) roleHostPairs

  (hosts :: Map Role (Set Addr)) <-
    ifLefts_map "hExprToAddrs called on HMap calculating hosts"
    $ M.mapWithKey roleHostCandidates found
  case null hosts of
    True -> Right S.empty
    False -> Right $ foldl1 S.intersection $ M.elems hosts

hExprToAddrs r s (HEval hm paths) = do
  (hosts :: Set Addr)     <- hExprToAddrs r s hm
  (its :: Set (Set Addr)) <-
    ifLefts_set "hExprToAddrs called on HEval, mapping over hosts"
    $ S.map (subExprs r paths) hosts
  Right $ S.unions its

hExprToAddrs _ s (HVar v) =
  maybe (Left $ keyErr "hExprToAddrs" v s) (Right . S.singleton)
  $ M.lookup v s

hExprToAddrs r _ (HExpr e) = S.singleton <$> exprToAddr r e

hExprToAddrs r s (HDiff base exclude) = do
  b <- prefixLeft "hExprToAddrs called on HDiff calculating base"
       $ hExprToAddrs r s base
  e <- prefixLeft "hExprToAddrs called on HDiff calculating exclude"
       $ hExprToAddrs r s exclude
  Right $ S.difference b e

-- | TRICK: For speed, put the most selective searches first in the list.
hExprToAddrs r s (HAnd hs) = foldr1 S.intersection <$>
                        ( ifLefts "hExprToAddrs called on HAnd"
                          $ map (hExprToAddrs r s) hs )

hExprToAddrs r s (HOr hs) = foldr1 S.union <$>
                       ( ifLefts "hExprToAddrs called on HOr"
                         $ map (hExprToAddrs r s) hs )
