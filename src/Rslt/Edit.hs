{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Rslt.Edit (
    exprToAddrInsert      -- ^ Rslt -> Expr   -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr] -> Either String (Rslt, [Addr])
  , replaceExpr       -- ^ Expr -> Addr -> Rslt -> Either String (Rslt, Addr)
  , replace           -- ^ RefExpr -> Addr -> Rslt      -> Either String Rslt
  , replaceInRole     -- ^ Role -> Addr -> Addr -> Rslt -> Either String Rslt
  , insert            -- ^ RefExpr -> Rslt              -> Either String Rslt
  , insertAt          -- ^ Addr -> RefExpr -> Rslt      -> Either String Rslt
  , deleteIfUnused    -- ^ Addr -> Rslt                 -> Either String Rslt
  ) where

import           Data.Functor (void)
import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.Index
import Rslt.RLookup
import Rslt.RTypes
import Rslt.RUtil
import Rslt.RValid
import Util.Misc


-- | = Edit + search

-- | `exprToAddrInsert r ei` returns the `Addr` containing ei, if present.
-- If not, it inserts ei, and then returns the `Addr` containing it.
-- Since it might modify the `Rslt`, it also returns that.
exprToAddrInsert :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert r ei = do
  let (mra :: Maybe Addr) = either (const Nothing) Just
                            $ exprToAddr r ei
  case mra of
    Just a -> Right (r, a)
    Nothing -> exprToAddrInsert_rootNotFound r ei


-- | `exprToAddrInsert_rootNotFound` is like `exprToAddrInsert`, in the case
-- that the root `RefExpr` has been determined not to be present,
-- but the others still might be.
exprToAddrInsert_rootNotFound :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert_rootNotFound _ (Addr a) =
  Left $ "exprToAddrInsert: Addr " ++ show a ++ "not found.\n"

exprToAddrInsert_rootNotFound r0 (Phrase w) = do
  a <- nextAddr r0
  r1 <- insertAt a (Phrase' w) r0
  Right (r1,a)

exprToAddrInsert_rootNotFound r0 (ExprTplt js) = do
  (r1,as) <- prefixLeft "exprToAddrInsert_rootNotFound"
            $ exprToAddrInsert_list r0 js
  a <- nextAddr r1
  r2 <- insertAt a (Tplt' $ as) r1
  Right (r2, a)

exprToAddrInsert_rootNotFound r0 (ExprRel (Rel ms t)) = do
  (r1,ta)  <- prefixLeft "exprToAddrInsert_rootNotFound"
            $ exprToAddrInsert r0 t
  (r2,mas) <- prefixLeft "exprToAddrInsert_rootNotFound"
             $ exprToAddrInsert_list r1 ms
  a <- nextAddr r2
  r3 <- insertAt a (Rel' $ Rel mas ta) r2
  Right (r3,a)


exprToAddrInsert_list :: Rslt -> [Expr] -> Either String (Rslt, [Addr])
exprToAddrInsert_list r0 is = do
  let ((er, as) :: (Either String Rslt, [Addr])) =
        L.mapAccumL f (Right r0) is where
        f :: Either String Rslt -> Expr -> (Either String Rslt, Addr)
        f (Left s) _ = (Left s, error "irrelevant")
        f (Right r) ei = case exprToAddrInsert r ei of
                           Left s -> (Left s, error "irrelevant")
                           Right (r',a) -> (Right r', a)
  r1 <- prefixLeft "exprToAddrInsert_list" er
  Right $ (r1, as)


-- | = Pure editing

replaceExpr :: Expr -> Addr -> Rslt -> Either String (Rslt, Addr)
replaceExpr e a r = prefixLeft "replaceExpr" $ do
  (r1 :: Rslt, a1 :: Addr) <- exprToAddrInsert r e
  rx1 :: RefExpr           <- addrToRefExpr r1 a1
  r2 :: Rslt               <- replace rx1 a r1
  Right (r2,a1)

replace :: RefExpr -> Addr -> Rslt -> Either String Rslt
replace re oldAddr r0 = prefixLeft "replace" $
  case refExprToAddr r0 re of
    Right newAddr -> do
      r2 <- _substitute newAddr oldAddr r0
      deleteIfUnused oldAddr r2
    Left _ -> do
      newAddr <- nextAddr r0
      _       <- validRefExpr r0 re
      r1      <- insertAt newAddr re r0
      r2      <- _substitute newAddr oldAddr r1
      deleteIfUnused oldAddr r2

_substitute :: Addr -> Addr -> Rslt -> Either String Rslt
_substitute new old r0 = do
  (roles :: Set (Role, Addr)) <- prefixLeft "_substitute"
                                 $ isIn r0 old
  let f :: Either String Rslt -> (Role, Addr) -> Either String Rslt
      f e@(Left _) _ = e
      f (Right r) (role,host) = replaceInRole role new host r
  S.foldl f (Right r0) roles

_replaceInRefExpr :: Rslt -> Role -> Addr -> RefExpr -> Either String RefExpr
_replaceInRefExpr r spot new host = prefixLeft "_replaceInRefExpr" $ do
  void $ addrToRefExpr r new

  case spot of
    RoleTplt -> case host of
      Rel' (Rel as _) -> do
        if variety r new == Right (TpltCtr, length as)
          then Right $ Rel' $ Rel as new
          else Left $ "_replaceInRefExpr: RefExpr at " ++ show new
                ++ " is not a valid Tplt in " ++ show host ++ ".\n"
      _ -> Left $ "_replaceInRefExpr: nothing plays the role of ExprTplt in "
                ++ show host ++ ".\n"

    RoleMember k -> do
      case host of

        Rel' (Rel as a) -> do
          as' <- replaceNth new k as
          Right $ Rel' $ Rel as' a

        Tplt' as -> do
          as' <- replaceNth new k as
          Right $ Tplt' as'

        _ -> Left $ "_replaceInRefExpr: RefExpr " ++ show host
             ++ " has no members.\n"

replaceInRole :: Role -> Addr -> Addr -> Rslt -> Either String Rslt
replaceInRole spot new host r = prefixLeft "replaceInRole" $ do
  _                          <- addrToRefExpr r new
  oldHostRefExpr             <- addrToRefExpr r host
  (hostHas :: Map Role Addr) <- has r host
  (old :: Addr) <- let err = Left $ "replaceInRole: RefExpr at " ++ show host
                             ++ " includes no position " ++ show spot ++ "\n."
    in maybe err Right $ M.lookup spot hostHas

  (newHostRefExpr :: RefExpr) <-
    _replaceInRefExpr r spot new oldHostRefExpr
  (newIsAlreadyIn :: Set (Role,Addr)) <- isIn r new

  Right $ r {
      _addrToRefExpr = M.insert host newHostRefExpr
                       $ _addrToRefExpr r
    , _refExprToAddr = M.insert newHostRefExpr host
                       $ M.delete oldHostRefExpr
                       $ _refExprToAddr r

    , _has    = M.adjust (M.insert spot new) host $ _has r

    , _isIn   =   M.filter (not . null)
      -- PITFALL: delete before inserting. Otherwise, replacing something
      -- with itself is not the identity operation.
                . M.insert new (S.insert (spot, host) newIsAlreadyIn)
      -- PITFALL: We can't adjust the value at new; it might not exist.
                . M.adjust (S.delete (spot, host)) old
                $ _isIn r
    }

insert :: RefExpr -> Rslt -> Either String Rslt
insert e r = do
  a <- prefixLeft "insert" $ nextAddr r
  insertAt a e r

-- | like `insert`, but specifying which `Addr` to give the new expression
insertAt :: Addr -> RefExpr -> Rslt -> Either String Rslt
insertAt a e r = do
  void $ prefixLeft "insertAt" $ validRefExpr r e
  let errMsg = "insertAt: Addr " ++ show a ++ " already occupied.\n"
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

-- | PITFALL: `_deleteInternalMentionsOf` could put the Rslt into an
-- invalid state, if it was run on an RefExpr that appears in other
-- RefExprs. This only deletes mentions in which it is the container
-- or "the thing", but not the contained.
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

deleteIfUnused :: Addr -> Rslt -> Either String Rslt
deleteIfUnused a r = do
  users <- prefixLeft "deleteIfUnused: " $ isIn r a
  if null users
    then _deleteInternalMentionsOf a r
    else Left $ "deleteIfUnused: Addr " ++ show a
         ++ " is used in other RefExprs.\n"
