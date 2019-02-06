{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Lookup where

import           Prelude hiding (lookup)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.Index
import Rslt.RTypes
import SeekSeq.Query.MkLeaf
import SeekSeq.Types
import Util


hExprToExpr :: HExpr -> Either String Expr
hExprToExpr (HExpr e) = Right e
hExprToExpr h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr constructor can be converted to an Expr.\n"


-- | == for building `Query`s

hFind :: HExpr -> Find Addr Rslt
hFind he = find $ flip hLookup he


-- | == `hLookup`: Lookup via the Hash language.

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


-- | = Some utilities

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


-- | == Lookup from `Expr`s

lookup :: Rslt -> Expr -> Either String Addr
lookup x img =
  let pel = prefixLeft "lookup"
  in case img of
  Word w -> pel $ addrOf x $ Word' w

  ExprAddr a -> pel (refExprAt x a) >>= const (Right a)

  Tplt is -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    pel $ addrOf x $ Tplt' mas

  Rel is i -> do
    mas <- ifLefts "lookup" $ map (lookup x) is
    ma <- pel $ lookup x i
    pel $ addrOf x (Rel' mas ma)

  Par _ _ -> Left $ "lookup: Pars are not in index, "
    ++ "cannot be looked up.\n"


-- | == Lookup from `Addr`s or `RefExpr`s. (These are convenience
-- functions for Map.lookup applied to an Rslt field.)

refExprAt :: Rslt -> Addr -> Either String RefExpr
refExprAt r a =
  maybe (Left $ "addrOf: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _refExprAt r

addrOf :: Rslt -> RefExpr -> Either String Addr
addrOf r e = maybe err Right $ M.lookup e $ _addrOf r
  where err = Left $ "addrOf: RefExpr " ++ show e ++ " not found.\n"

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = do
  either (\s -> Left $ "has: " ++ s) Right $ refExprAt r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a = do
  either (\s -> Left $ "isIn: " ++ s) Right $ refExprAt r a
  maybe (Right S.empty) Right $ M.lookup a $ _isIn r

-- | `fills r (role,a)` finds the expression that occupies
-- role in a.
fills :: Rslt -> (Role, Addr) -> Either String Addr
fills x (r,a) = do
  (positions :: Map Role Addr) <-
    prefixLeft "fills" $ has x a
  let err = Left $ "fills: role " ++ show r
            ++ " not among positions in RefExpr at " ++ show a
  maybe err Right $ M.lookup r positions

