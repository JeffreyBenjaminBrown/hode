{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Lookup where

import           Data.Functor (void)
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes
import Qseq.MkLeaf
import Rslt.RTypes
import Rslt.RUtil
import Util.Misc


-- | == build `Query`s for `Rslt`s

hFind :: HExpr -> Find Addr Rslt
hFind he = Find f $ hVars he
  where f rslt subst = hExprToAddrs rslt subst he

hFindSubExprs :: [[Role]] -> Either Addr Var -> Find Addr Rslt
hFindSubExprs paths = mkFindFrom f where
  f :: Rslt -> Addr -> Either String (Set Addr)
  f r a = subExprs r paths a


-- | HExpr -> Expr

hExprToExpr :: Rslt -> HExpr -> Either String Expr
hExprToExpr _ (HExpr e) = Right e
hExprToExpr r h@(HMap mh) = do
  (me :: Map Role Expr) <- ifLefts_map "hExprToExpr"
    $ M.map (hExprToExpr r) mh
  (t :: Expr) <-
    maybe (Left $ "hExprToExpr: no template in " ++ show h)
    Right $ M.lookup RoleTplt me
  case t of Tplt _ -> Right ()
            x -> Left $ "hExprToExpr: in " ++ show h
                 ++ ", the expression " ++ show x ++ " is not a Tplt."
  ta <- prefixLeft "hExprToExpr" $ arity r t
  if M.size me == ta then Right ()
    else Left $ "hExprToExpr: arity mismatch between " ++ show h
         ++ " and its Tplt " ++ show t
  Right $ Rel (sort $ M.elems $ M.delete RoleTplt me) t

hExprToExpr _ h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr and HMap constructors can be so converted."


-- | Expr from RefExpr

refExprToExpr :: Rslt -> RefExpr -> Either String Expr
refExprToExpr _ (Word' w) = Right $ Word w
refExprToExpr r (Tplt' jointAs) = do
  (jointEs  :: [RefExpr])   <-
    ifLefts "refExprToExpr" $ map (addrToRefExpr r) jointAs
  (jointEis :: [Expr]) <-
    ifLefts "refExprToExpr" $ map (refExprToExpr r) jointEs
  Right $ Tplt jointEis

refExprToExpr r (Rel' memAs tA) = do
  (memEs  :: [RefExpr]) <- ifLefts    "refExprToExpr"
                          $ map (addrToRefExpr r) memAs
  (memEis :: [Expr])    <- ifLefts    "refExprToExpr"
                           $ map (refExprToExpr r) memEs
  (tE     :: RefExpr)   <- prefixLeft "refExprToExpr"
                           $ addrToRefExpr r tA
  (tEi    :: Expr)      <- prefixLeft "refExprToExpr"
                           $ refExprToExpr r tE
  Right $ Rel memEis tEi

refExprToExpr r (Par' sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr]) <- ifLefts "refExprToExpr" $ map (addrToRefExpr r) as
  (eis :: [Expr])    <- ifLefts "refExprToExpr" $ map (refExprToExpr r) es
  Right $ Par (zip ss eis) s


-- | == `hExprToAddrs`: Lookup via the Hash language.

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
    ( ifLefts_set "hExprToAddrs called on HEval, mapping over hosts"
      $ S.map (subExprs r paths) hosts )
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


-- | = Find sub-`Expr`s of an `Expr`

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


-- | == Lookup from an `Expr`

exprToAddr :: Rslt -> Expr -> Either String Addr
exprToAddr x img =
  let pel = prefixLeft "exprToAddr"
  in case img of
  Word w -> pel $ refExprToAddr x $ Word' w

  Addr a -> pel (addrToRefExpr x a) >>= const (Right a)

  Tplt is -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    pel $ refExprToAddr x $ Tplt' mas

  Rel is i -> do
    mas <- ifLefts "exprToAddr" $ map (exprToAddr x) is
    ma <- pel $ exprToAddr x i
    pel $ refExprToAddr x (Rel' mas ma)

  Par _ _ -> Left $ "exprToAddr: Pars are not in index, "
    ++ "cannot be looked up.\n"


-- | == Lookup from `Addr`s or `RefExpr`s. (These are convenience
-- functions for Map.exprToAddr applied to an Rslt field.)

addrToRefExpr :: Rslt -> Addr -> Either String RefExpr
addrToRefExpr r a =
  maybe (Left $ "addrToRefExpr: Addr " ++ show a ++ " absent.\n") Right
  $ M.lookup a $ _addrToRefExpr r

addrToExpr :: Rslt -> Addr -> Either String Expr
addrToExpr r a = addrToRefExpr r a >>= refExprToExpr r

refExprToAddr :: Rslt -> RefExpr -> Either String Addr
refExprToAddr r e = maybe err Right $ M.lookup e $ _refExprToAddr r
  where err = Left $ "refExprToAddr: RefExpr " ++ show e ++ " not found.\n"

variety :: Rslt -> Addr -> Either String (ExprCtr, Arity)
variety r a = maybe err Right $ M.lookup a $ _variety r
  where err = Left $ "variety: Addr " ++ show a ++ " not found.\n"

arity :: Rslt -> Expr -> Either String Arity
arity r (Addr a)  = snd <$> variety r a
arity _ (Word _)  = Right 0
arity r (Rel ms t) = do
  ta <- arity r t
  if ta == length ms then Right ta
    else Left $ "arity: Rel Tplt " ++ show t
         ++ " does not match number of Rel members " ++ show ms ++ ".\n"
arity _ (Tplt x)  = Right $ length x - 1
arity _ (Par x _) = Right $ length x


-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Either String (Map Role Addr)
has r a = do
  void $ either (\s -> Left $ "has: " ++ s) Right $ addrToRefExpr r a
  maybe (Right M.empty) Right $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Either String (Set (Role,Addr))
isIn r a = do
  void $ either (\s -> Left $ "isIn: " ++ s) Right $ addrToRefExpr r a
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
