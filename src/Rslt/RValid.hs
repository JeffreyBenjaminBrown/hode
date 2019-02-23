-- | Gory details, not part of the Rslt interface.

{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RValid where

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
validExpr r (Addr a) = allAddrsPresent r [a]
validExpr _ (Word _) = Right ()

validExpr r rel@(Rel ms t) = do
  let err = "validExpr called on Rel" ++ show rel
  void $ ifLefts err $ map (validExpr r) $ t : ms
  ((tc,ta) :: (ExprCtr,Arity)) <-
    prefixLeft err $ exprToAddr r t >>= variety r
  (te :: Expr) <-    exprToAddr r t >>= addrToExpr r
    -- looks silly, but it ensures te is not an `Addr`
  if tc == TpltCtr   then Right ()
    else Left $ err ++ ": non-template in template position."
  if ta == length ms then Right ()
    else Left $ err ++ " with template " ++ show te ++ ": arity mismatch."

validExpr r t@(Tplt js) =
  ( ifLefts ("validExpr called on Tplt " ++ show t)
    $ map (validExpr r) js )
  >> return ()
validExpr r (Par pairs _) =
  ( ifLefts "validExpr called on a Par"
    $ map (validExpr r) $ map snd pairs )
  >> return ()


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
       Word' _             -> Right ()


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
  involved (Word' _)    = error "impossible"
  involved (Tplt' as)   = as
  involved (Rel' as a)  = a : as
  involved (Par' sas _) = map snd sas

  collections :: Map Addr RefExpr
  collections = M.filter isCollection $ _addrToRefExpr r where
    isCollection expr = case expr of Word' _ -> False
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
