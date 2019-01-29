-- | Gory details, not part of the Rslt interface.

module Data.Rslt.RValid where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.RTypes
import Data.Rslt
import Util


-- | == Check an Expr

--validExpr :: Rslt -> Expr ->

-- | `validIfRel e`, if e is a Rel, is true if the address in the Tplt
-- position of e really corresponds to a Tplt in r, and that Tplt
-- has the right Arity.
validIfRel :: Rslt -> Expr -> Either String ()
validIfRel r (Rel aMembers aTplt) = do
  (ctr,ar) <- let
    msg = "validIfRel: nothing (hence no template) at " ++ show aTplt ++ ".\n"
    in maybe (Left msg) Right $ varieties r aTplt
  if ctr == Tplt' then Right ()
    else Left $ "validIfRel: expr at " ++ show aTplt ++ " not a Tplt.\n"
  if ar == length aMembers then Right ()
    else Left $ "validIfRel: expr at " ++ show aTplt
    ++ " does not match arity of " ++ show aMembers ++ ".\n"
validIfRel _ _ = Right ()

allReferencesExist :: Rslt -> Expr -> Either String ()
allReferencesExist _ (Word _) = Right ()
allReferencesExist r e = let
  f :: [Addr] -> Either String ()
  f as = case _allReferencesExist r as of
    Right () -> Right ()
    Left as -> Left $ "allReferencesExist: Addr values not present in Rslt: "
               ++ show as
  in case e of
       Rel aMembers aTplt -> f $ aTplt : aMembers
       Tplt as            -> f as
       Par sas _          -> f $ map snd sas

_allReferencesExist :: Rslt -> [Addr] -> Either [Addr] ()
_allReferencesExist r as =
  let lookups = zip as $ map (exprAt r) as
      nothings = filter (isNothing . snd) lookups
  in if null nothings then Right ()
     else Left $ map fst nothings

-- | == Check the database

collectionsWithAbsentAddrs :: Exprs -> Rslt -> Map Addr [Addr]
collectionsWithAbsentAddrs exprs r = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . flip M.lookup (_varieties r)

  involved :: Expr -> [Addr]
  involved (Word _)    = error "impossible"
  involved (Tplt as)   = as
  involved (Rel as a)  = a : as
  involved (Par sas _) = map snd sas

  collections :: Exprs
  collections = M.filter isCollection exprs where
    isCollection expr = case expr of Word _ -> False
                                     _      -> True

relsWithoutMatchingTplts :: Exprs -> Rslt -> Exprs
relsWithoutMatchingTplts exprs r = res where
  res = M.filter (not . relMatchesTpltArity) rels

  relMatchesTpltArity :: Expr -> Bool
  relMatchesTpltArity e@(Rel _ t) = case M.lookup t $ _varieties r of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False
  relMatchesTpltArity _ = error "relMatchesTpltArity: impossible."

  rels = M.filter isRel exprs where
    isRel (Rel _ _) = True
    isRel _         = False
