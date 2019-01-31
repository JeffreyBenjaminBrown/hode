{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Data.Rslt.Edit where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.Index
import Data.Rslt.Lookup
import Data.Rslt.RTypes
import Data.Rslt.RValid
import Util


-- | Edit

replace :: Expr -> Addr -> Rslt -> Either String Rslt
replace e oldAddr r = do
  let newAddr = maxAddr r + 1
      prependEither = either (\s -> Left $ "substitute: " ++ s) Right
  _ <-     prependEither $ validExpr r e
  r <-     prependEither $ insertAt newAddr e r
  r <-     prependEither $ _substitute newAddr oldAddr r
  id $     prependEither $ deleteUnusedExpr oldAddr r

_substitute :: Addr -> Addr -> Rslt -> Either String Rslt
_substitute new old r = do
  (roles :: Set (Role, Addr)) <-
    let errMsg = "_substitute: Addr " ++ show old ++ " not present.\n"
    in maybe (Left errMsg) Right $ isIn r old
  let f :: Either String Rslt -> (Role, Addr) -> Either String Rslt
      f e@(Left _) _ = e
      f (Right r) (role,host) = replaceInRole role new host r
  S.foldl f (Right r) roles

_replaceInExpr :: Rslt -> Role -> Addr -> Expr -> Either String Expr
_replaceInExpr r spot new host = do
  let msg = "_replaceInExpr: Addr " ++ show new ++ " not present.\n"
    in maybe (Left msg) Right $ exprAt r new

  case spot of
    RoleTplt -> case host of
      Rel as _ -> do
        if variety r new == Just (Tplt', length as)
          then Right $ Rel as new
          else Left $ "_replaceInExpr: Expr at " ++ show new
               ++ " is not a valid Tplt in " ++ show host ++ ".\n"
      _ -> Left $ "_replaceInExpr: nothing plays the role of Tplt in "
           ++ show host ++ ".\n"

    RoleMember k -> do
      let errFunc s = Left $ "_replaceInExpr: error in callee:\n" ++ s
      case host of

        Rel as a -> do
          as' <- either errFunc Right $ replaceNth new k as
          Right $ Rel as' a

        Tplt as -> do
          as' <- either errFunc Right $ replaceNth new k as
          Right $ Tplt as'

        Par sas s -> do
          let (ss,as) = unzip sas
          as' <- either errFunc Right $ replaceNth new k as
          Right $ Par (zip ss as') s
        _ -> Left $ "_replaceInExpr: Expr " ++ show host
             ++ " has no members.\n"

replaceInRole :: Role -> Addr -> Addr -> Rslt -> Either String Rslt
replaceInRole spot new host r = do
  let (errorAbsent :: Addr -> String) = \addr ->
        "replaceInRole: Addr " ++ show addr ++ " not present.\n"
  _           <- maybe (Left $ errorAbsent new)  Right $ exprAt r new
  oldHostExpr <- maybe (Left $ errorAbsent host) Right $ exprAt r host
  let (hostHas :: Map Role Addr) =
        maybe (error "impossible") id $ has r host
  if elem spot $ M.keysSet hostHas then Right ()
    else Left $ "replaceInRole: Expr at "
         ++ show host ++ " includes no position " ++ show spot ++ "\n."
  let (old :: Addr) = maybe (error "impossible") id $ M.lookup spot hostHas

  (newHostExpr :: Expr) <-
    either (\s -> Left $ "replaceInRole: error in callee:\n" ++ s) Right
    $ _replaceInExpr r spot new oldHostExpr
  let (newIsAlreadyIn :: Set (Role,Addr)) =
        maybe S.empty id $ isIn r new

  Right $ r {
      _exprAt = M.insert host newHostExpr $ _exprAt r
    , _addrOf = let f = case newHostExpr of
                          Par _ _ -> id
                          _       -> M.insert newHostExpr host
                in f $ M.delete oldHostExpr $ _addrOf r

    , _has    = M.adjust (M.insert spot new) host $ _has r

    , _isIn   =   M.filter (not . null)
      -- PITFALL: delete before inserting. Otherwise, replacing something
      -- with itself is not the identity operation.
                . M.insert new (S.insert (spot, host) newIsAlreadyIn)
      -- PITFALL: We can't adjust the value at new; it might not exist.
                . M.adjust (S.delete (spot, host)) old
                $ _isIn r
    }

insert :: Expr -> Rslt -> Either String Rslt
insert e r = insertAt (maxAddr r + 1) e r

insertAt :: Addr -> Expr -> Rslt -> Either String Rslt
insertAt a e r = do
  either (\s -> Left $ "insert: " ++ s) Right
    $ validExpr r e
  let errMsg = "insert: Addr " ++ show a ++ " already occupied.\n"
      in maybe (Right ()) (const $ Left errMsg)
         $ exprAt r a
  Right $ _insert a e r

-- | PITFALL: Unsafe. Checks neither that the Expr is valid, nor that
-- the Addr collides with nothing already present.
_insert :: Addr -> Expr -> Rslt -> Rslt
_insert a e r = Rslt {
    _exprAt = M.insert a e $ _exprAt r
  , _addrOf = M.insert e a $ _addrOf r
  , _variety = M.insert a (exprVariety e) $ _variety r
  , _has = let
      (positions :: Map Role Addr) = M.fromList $ exprPositions e
      in if null positions then _has r
         else M.insert a positions $ _has r
  , _isIn = invertAndAddPositions (_isIn r) (a, exprPositions e)
  , maxAddr = if a > maxAddr r then a
              else maxAddr r
  }

-- | PITFALL: One could put the Rslt into an invalid state by running this
-- on an Expr that appears in other Exprs. This only deletes mentions in
-- which it is the container or "the thing", but not the contained.
_deleteInternalMentionsOf :: Addr -> Rslt -> Rslt
_deleteInternalMentionsOf a r = let
  (aHas   :: Maybe (Map Role Addr))       = has r a
  (_has2  :: Map Addr (Map Role Addr))    = M.delete a $ _has r

  (_isIn1 :: Map Addr (Set (Role, Addr))) = _isIn r
  (_isIn2 :: Map Addr (Set (Role, Addr))) =
    M.filter (not . null)
    $ maybe _isIn1 (M.foldlWithKey f _isIn1) aHas
    where
      f :: Map Addr (Set (Role, Addr)) -> Role -> Addr
        -> Map Addr (Set (Role, Addr))
      f ii rl ad = M.adjust (S.delete (rl,a)) ad ii

  in Rslt { _has  = _has2
          , _isIn = _isIn2
          , _variety = M.delete a $ _variety r
          , _exprAt = M.delete a $ _exprAt r
          , _addrOf = let
              e = maybe (error "imposible") id $ exprAt r a
              in M.delete e $ _addrOf r
          , maxAddr = if a == maxAddr r
                      then maybe 0 id $ S.lookupMax $ M.keysSet $ _exprAt r
                      else maxAddr r
       }

deleteUnusedExpr :: Addr -> Rslt -> Either String Rslt
deleteUnusedExpr a r = case isIn r a of
  Nothing -> Left $ "deleteUnused: Addr " ++ show a ++ " not present.\n"
  Just s -> if null s
    then Right $ _deleteInternalMentionsOf a r
    else Left $ "deleteUnused: Addr " ++ show a
         ++ " is used in other Exprs.\n"

