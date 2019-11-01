-- Everything else in Hode.Rslt.Edit depends on this.

{-# LANGUAGE
ScopedTypeVariables,
LambdaCase #-}

module Hode.Rslt.Edit.Initial (
    renameAddr_unsafe  -- ^ Addr -> Addr -> Rslt -> Rslt
  , _replaceInRefExpr  -- ^ Rslt -> Role -> Addr -> RefExpr
                      -- -> Either String RefExpr
  , insert         -- ^ RefExpr ->              Rslt -> Either String Rslt
  , insertAt       -- ^ Addr -> RefExpr ->      Rslt -> Either String Rslt
  , deleteIfUnused -- ^ Addr ->                 Rslt -> Either String Rslt
  ) where

import           Control.Lens hiding (has, re)
import           Data.Functor (void)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Rslt.Index
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.RValid
import Hode.Util.Misc


-- | `renameAddr_unsafe` could do harm, if the new `Addr` was
-- already present. However, that might also be appropriate --
-- for instance, after having replaced an old `Expr` at `old` with
-- a new one at `new`, or after having discovered a duplicate.
renameAddr_unsafe :: Addr -> Addr -> Rslt -> Rslt
renameAddr_unsafe old new r = let
  aa :: Addr -> Addr
  aa a = if a == old then new else a

  rere :: RefExpr -> RefExpr
  rere e@(Phrase' _) = e
  rere (Rel' rel)    = Rel' $ fmap aa rel
  rere (Tplt' tplt)  = Tplt' $ fmap aa tplt

  in Rslt { _addrToRefExpr = _addrToRefExpr r &
                             M.mapKeys aa . M.map rere
          , _refExprToAddr = _refExprToAddr r &
                             M.mapKeys rere . M.map aa
          , _variety = _variety r &
                       M.mapKeys aa
          , _has = _has r &
                   M.mapKeys aa . M.map (M.map aa)
          , _isIn = _isIn r &
                    M.mapKeys aa . M.map (S.map $ _2 %~ aa)
          }

_replaceInRefExpr :: Rslt -> Role -> Addr -> RefExpr
                  -> Either String RefExpr
_replaceInRefExpr r spot new host = let

  f :: Role -> RefExpr -> Either String RefExpr
  f (RoleInTplt' rol) (Tplt' t) = do
    t' <- replaceInTplt new rol t
    Right $ Tplt' t'
  f (RoleInRel' rol) (Rel' rel) = do
    if variety r new == Right (TpltCtr, arity rel)
      then Rel' <$> replaceInRel new rol rel
      else Left $ "Arity mismatch: RefExpr at " ++ show new
           ++ " is not a valid Tplt in " ++ show host ++ ".\n"

  in
  prefixLeft "_replaceInRefExpr: " $ do
  void $ addrToRefExpr r new
  f spot host

insert :: RefExpr -> Rslt -> Either String Rslt
insert e r = do
  a <- prefixLeft "insert" $ nextAddr r
  insertAt a e r

-- | like `insert`, but specifying which `Addr` to give the new expression
insertAt :: Addr -> RefExpr -> Rslt -> Either String Rslt
insertAt a e r = do
  void $ prefixLeft "insertAt: " $ validRefExpr r e
  let errMsg = "Addr " ++ show a ++ " already occupied.\n"
      in void $ either Right (const $ Left errMsg)
         $ addrToRefExpr r a
  Right $ _insert a e r

-- | PITFALL: Unsafe. Checks neither that the RefExpr is valid, nor that
-- the Addr collides with nothing already present.
_insert :: Addr -> RefExpr -> Rslt -> Rslt
_insert a e r = Rslt {
    _addrToRefExpr = M.insert a e $ _addrToRefExpr r
  , _refExprToAddr = M.insert e a $ _refExprToAddr r
  , _variety = M.insert a (refExprVariety e) $ _variety r
  , _has = let
      (positions :: Map Role Addr) = M.fromList $ refExprPositions e
      in if null positions then _has r
         else M.insert a positions $ _has r
  , _isIn = invertAndAddPositions (_isIn r) (a, refExprPositions e)
  }

deleteIfUnused :: Addr -> Rslt -> Either String Rslt
deleteIfUnused a r = do
  users <- prefixLeft "deleteIfUnused: " $ isIn r a
  if null users
    then _deleteInternalMentionsOf a r
    else Left $ "deleteIfUnused: Addr " ++ show a
         ++ " is used in other RefExprs.\n"

-- | PITFALL: `_deleteInternalMentionsOf` could put the Rslt into an
-- invalid state, if it was run on an RefExpr that appears in other
-- RefExprs. This only deletes mentions in which it is the container
-- or "the thing" (hence "internal" mentions), but not the contained.
_deleteInternalMentionsOf :: Addr -> Rslt -> Either String Rslt
_deleteInternalMentionsOf a r = do
  (aHas       ::           Map Role Addr) <-
    prefixLeft "_deleteInternalMentionsOf" $ has r a
  let (_has2  :: Map Addr (Map Role Addr)) = M.delete a $ _has r
      (_isIn1 :: Map Addr (Set (Role, Addr))) = _isIn r
      (_isIn2 :: Map Addr (Set (Role, Addr))) =
        M.filter (not . null)
        $ M.foldlWithKey f _isIn1 aHas
        where
          f :: Map Addr (Set (Role, Addr)) -> Role -> Addr
            -> Map Addr (Set (Role, Addr))
          f ii rl ad = M.adjust (S.delete (rl,a)) ad ii

  _refExprToAddr2 <- do
    e <- prefixLeft "_deleteInternalMentionsOf" $ addrToRefExpr r a
    Right $ M.delete e $ _refExprToAddr r

  Right $ Rslt {
    _has  = _has2
    , _isIn = _isIn2
    , _variety = M.delete a $ _variety r
    , _addrToRefExpr = M.delete a $ _addrToRefExpr r
    , _refExprToAddr = _refExprToAddr2
    }
