-- | Gory details, not part of the Rslt interface.

{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RValid where

import           Data.Functor.Foldable
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Functor (void)

import Rslt.RLookup
import Rslt.RTypes
import Rslt.RUtil
import Util.Misc


-- | == Check an `Expr`

validExpr :: Rslt -> Expr -> Either String ()
validExpr r = para f where
  f :: Base Expr (Expr, Either String ()) -> Either String ()
  f (AddrF a)   = allAddrsPresent r [a]
  f (PhraseF _) = Right ()

  f rel@(RelF memEis (t,te)) = do
    let (ms :: [Expr], es :: [Either String ()]) = unzip memEis
        err = "validExpr called on " ++ show (embed $ fmap fst rel)
    void $ ifLefts err $ te : es
    ((tc,ta) :: (ExprCtr,Arity)) <-
      prefixLeft err $ exprToAddr r t >>= variety r
    (tx :: Expr) <-    exprToAddr r t >>= addrToExpr r
      -- looks silly, but this ensures tx is not an `Addr`
    if tc == TpltCtr   then Right ()
      else Left $ err ++ ": non-template in template position."
    if ta == length ms then Right ()
      else Left $ err ++ " with template " ++ show tx ++ ": arity mismatch."

  f (TpltF js)     = ifLefts err (map snd js)
                     >> return ()
    where err = "validExpr called on " ++ show (Tplt $ map fst js)
  f (ParF pairs _) = ifLefts err (map (snd . snd) pairs)
                     >> return ()
    where err = "validExpr called on a Par."


-- | == Check a `RefExpr`

validRefExpr :: Rslt -> RefExpr -> Either String ()
validRefExpr r e = do validTplt r e
                      refExprRefsExist r e

-- | `validIfRel e`, if e is a Rel, is true if the address in the Tplt
-- position of e really corresponds to a Tplt in r, and that Tplt
-- has the right Arity.
validTplt :: Rslt -> RefExpr -> Either String ()
validTplt r (Rel' aMembers aTplt) = do
  (ctr,ar) <- prefixLeft "validTplt" $ variety r aTplt
  if ctr == TpltCtr        then Right ()
    else Left $ "validTplt: expr at " ++ show aTplt ++ " not a Tplt.\n"
  if ar == length aMembers then Right ()
    else Left $ "validTplt: expr at " ++ show aTplt
    ++ " does not match arity of " ++ show aMembers ++ ".\n"
validTplt _ _ = Right ()

refExprRefsExist :: Rslt -> RefExpr -> Either String ()
refExprRefsExist r e = let
  f :: [Addr] -> Either String ()
  f as = case allAddrsPresent r as of
    Right () -> Right ()
    Left absent -> Left
      $ "refExprRefsExist: These Addrs are absent: " ++ show absent
  in case e of
       Rel' aMembers aTplt -> f $ aTplt : aMembers
       Tplt' as            -> f as
       Par' sas _          -> f $ map snd sas
       Phrase' _             -> Right ()


-- | == Check the database

validRslt :: Rslt -> Either String ()
validRslt r = do
  let unmatched = relsWithoutMatchingTplts r
      in if null unmatched then Right ()
         else Left $ "validRslt: rels without matching templates:\n"
              ++ show unmatched ++ ".\n"
  let unfillable = collectionsWithAbsentAddrs r
      in if null unfillable then Right ()
         else Left $ "validRslt: collections with absent Addrs:\n"
              ++ show unfillable

collectionsWithAbsentAddrs :: Rslt -> Map Addr [Addr]
collectionsWithAbsentAddrs r = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . flip M.lookup (_variety r)

  involved :: RefExpr -> [Addr]
  involved (Phrase' _)    = error "impossible"
  involved (Tplt' as)   = as
  involved (Rel' as a)  = a : as
  involved (Par' sas _) = map snd sas

  collections :: Map Addr RefExpr
  collections = M.filter isCollection $ _addrToRefExpr r where
    isCollection expr = case expr of Phrase' _ -> False
                                     _       -> True

relsWithoutMatchingTplts :: Rslt -> Map Addr RefExpr
relsWithoutMatchingTplts r = res where
  res = M.filter (not . relMatchesTpltArity) rels

  relMatchesTpltArity :: RefExpr -> Bool
  relMatchesTpltArity e@(Rel' _ t) = case M.lookup t $ _variety r of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      TpltCtr -> refExprArity e == art
      _       -> False
  relMatchesTpltArity _ = error "relMatchesTpltArity: impossible."

  rels = M.filter isRel $ _addrToRefExpr r where
    isRel (Rel' _ _) = True
    isRel _         = False


-- | = A utility

allAddrsPresent :: Rslt -> [Addr] -> Either String ()
allAddrsPresent r as = do
  void $ ifLefts "allAddrsPresent: " $ map (addrToRefExpr r) as
  Right ()
