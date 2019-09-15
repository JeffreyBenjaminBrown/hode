-- | Gory details, not part of the Rslt interface.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.RValid (
  -- | = validate individual expressions
  verifyVariety -- ^ Rslt -> Addr -> (Maybe ExprCtr, Maybe Arity)
                -- -> Either String ()
  , validExpr                  -- ^ Rslt -> Expr    -> Either String ()
  , validRefExpr               -- ^ Rslt -> RefExpr -> Either String ()
  , validTplt                  -- ^ Rslt -> RefExpr -> Either String ()
  , refExprRefsExist           -- ^ Rslt -> RefExpr -> Either String ()

-- | = validate an entire `Rslt`
  , validRslt                  -- ^ Rslt            -> Either String ()
  , collectionsWithAbsentAddrs -- ^ Rslt -> Map Addr [Addr]
  , relsWithoutMatchingTplts   -- ^ Rslt -> Map Addr RefExpr
  , allAddrsPresent            -- ^ Rslt -> [Addr]  -> Either String ()
  ) where

import           Data.Functor.Foldable
import           Data.Foldable (toList)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Functor (void)

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc


-- | == Check an `Expr`

verifyVariety :: Rslt -> Addr -> (Maybe ExprCtr, Maybe Arity)
              -> Either String ()
verifyVariety r a (mc,ma) = do
  (ctr,ar) <- variety r a
  if mc == Nothing then Right () else
    if mc == Just ctr then Right () else
      Left $ "Expr at " ++ show a ++ " does not match " ++ show ctr
  if ma == Nothing then Right () else
    if ma == Just ar then Right () else
      Left $ "Expr at " ++ show a ++ " does not have arity " ++ show ma

validExpr :: Rslt -> Expr -> Either String ()
validExpr r = prefixLeft "validExpr: " . para f where
  f :: Base Expr (Expr, Either String ()) -> Either String ()
  f (AddrF a)   = allAddrsPresent r [a]
  f (PhraseF _) = Right ()

  f rel@(ExprRelF (Rel memEis (t,te))) = do
    let (ms :: [Expr], es :: [Either String ()]) = unzip memEis
        err = "called on " ++ show (embed $ fmap fst rel)
    void $ prefixLeft err $ ifLefts $ te : es
    (tc,ta) :: (ExprCtr,Arity) <-
      prefixLeft err $ exprToAddr r t >>= variety r
    (tx :: Expr) <-    exprToAddr r t >>= addrToExpr r
      -- looks silly, but this ensures tx is not an `Addr`
    if tc == TpltCtr   then Right ()
      else Left $ err ++ ": non-Tplt in Tplt position."
    if ta == length ms then Right ()
      else Left $ err ++ " with Tplt " ++ show tx ++ ": arity mismatch."

  f (ExprTpltF t@(Tplt a bs c)) =
    prefixLeft (", called on " ++ show (ExprTplt $ fmap fst t) ++ ": ")
    ( ifLefts (fmap snd t) >>
      if null a && null bs && null c
      then Left "null Tplt."
      else return () )


-- | == Check a `RefExpr`

validRefExpr :: Rslt -> RefExpr -> Either String ()
validRefExpr r e = prefixLeft "validRefExpr: " $
  validTplt r e >>
  refExprRefsExist r e

-- | `validIfRel e`, if e is a Rel, is true if the address in the Tplt
-- position of e really corresponds to a Tplt in r, and that Tplt
-- has the right Arity.
validTplt :: Rslt -> RefExpr -> Either String ()
validTplt r (Rel' (Rel aMembers aTplt)) =
  prefixLeft "validTplt: " $ do
    (ctr,ar) <- variety r aTplt
    if ctr == TpltCtr        then Right ()
      else Left $ "expr at " ++ show aTplt ++ " not a Tplt.\n"
    if ar == length aMembers then Right ()
      else Left $ "expr at " ++ show aTplt
           ++ " does not match arity of " ++ show aMembers ++ ".\n"
validTplt r (Tplt' (Tplt Nothing [] Nothing)) =
  Left "validTplt: Null Tplt."
validTplt _ _ = Right ()

refExprRefsExist :: Rslt -> RefExpr -> Either String ()
refExprRefsExist r e = prefixLeft "refExprRefsExist: " $ let
  f :: [Addr] -> Either String ()
  f as = case allAddrsPresent r as of
    Right () -> Right ()
    Left absent -> Left
      $ "These Addrs are absent: " ++ show absent
  in case e of Rel' rel  -> f $ toList rel
               Tplt' t   -> f $ toList t
               Phrase' _ -> Right ()


-- | == Check the database

validRslt :: Rslt -> Either String ()
validRslt r = prefixLeft "validRslt: " $ do
  let unmatched = relsWithoutMatchingTplts r
      in if null unmatched then Right ()
         else Left $ "rels without matching Tplts:\n"
              ++ show unmatched ++ ".\n"
  let unfillable = collectionsWithAbsentAddrs r
      in if null unfillable then Right ()
         else Left $ "collections with absent Addrs:\n"
              ++ show unfillable

collectionsWithAbsentAddrs :: Rslt -> Map Addr [Addr]
collectionsWithAbsentAddrs r = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . flip M.lookup (_variety r)

  involved :: RefExpr -> [Addr]
  involved (Phrase' _) = error "impossible"
  involved (Tplt' t)   = toList t
  involved (Rel' rel)  = toList rel

  collections :: Map Addr RefExpr
  collections = M.filter isCollection $ _addrToRefExpr r where
    isCollection expr = case expr of Phrase' _ -> False
                                     _         -> True

relsWithoutMatchingTplts :: Rslt -> Map Addr RefExpr
relsWithoutMatchingTplts r = res where
  res = M.filter (not . relMatchesTpltArity) rels

  relMatchesTpltArity :: RefExpr -> Bool
  relMatchesTpltArity e@(Rel' (Rel _ t)) =
    case M.lookup t $ _variety r of
      Nothing         -> False
      Just (ctr, art) -> case ctr of
        TpltCtr -> refExprArity e == art
        _       -> False
  relMatchesTpltArity _ =
    error "relMatchesTpltArity: impossible."

  rels = M.filter isRel $ _addrToRefExpr r where
    isRel (Rel' _) = True
    isRel _        = False


-- | = A utility

allAddrsPresent :: Rslt -> [Addr] -> Either String ()
allAddrsPresent r as =
  prefixLeft "allAddrsPresent: "
  $ void (ifLefts $ map (addrToRefExpr r) as)
  >> Right ()
