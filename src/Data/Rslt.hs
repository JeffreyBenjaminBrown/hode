{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.Index
import Data.Rslt.RTypes
import Util


mkRslt :: Exprs -> Rslt
mkRslt es = let
  (hasMap :: Map Addr (Map Role Addr)) =
    M.filter (not . M.null)
    $ M.map (M.fromList . exprPositions)
    $ es
  in Rslt {
    _exprAt = es
  , _addrOf = imgDb es
  , _variety = M.map exprVariety es
  , _has = hasMap
  , _isIn = foldl invertAndAddPositions M.empty
            $ M.toList $ M.map M.toList hasMap
  , maxAddr = maybe 0 id
              $ S.lookupMax $ M.keysSet es
  }


-- | Edit

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
  let (h :: Map Role Addr) =
        maybe (error "impossible") id $ has r host
  if elem spot $ M.keysSet h then Right ()
    else Left $ "replaceInRole: Expr at "
         ++ show host ++ " includes no position " ++ show spot ++ "\n."
  let (old :: Addr) = maybe (error "impossible") id $ M.lookup spot h

  (newHostExpr :: Expr) <-
    either (\s -> Left $ "replaceInRole: error in callee:\n" ++ s) Right
    $ _replaceInExpr r spot new oldHostExpr
  Right $ r {
      _exprAt = M.insert host newHostExpr $ _exprAt r
    , _addrOf = M.insert newHostExpr host $ _addrOf r
    , _has    = M.adjust (M.insert spot new) host $ _has r
    , _isIn   = M.filter (not . null)
      -- PITFALL: delete before inserting. Otherwise replacing something with itself
      -- is not the identity operation
                . M.adjust (S.insert (spot, host)) new
                . M.adjust (S.delete (spot, host)) old
                $ _isIn r }

insert :: Addr -> Expr -> Rslt -> Rslt
insert a e r = Rslt {
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
    maybe _isIn1 (M.foldlWithKey f _isIn1) aHas
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
    else Left $ "deleteUnused: Addr " ++ show a ++ " is used in other Exprs.\n"


-- | = Search

lookup :: Rslt -> ImgOfExpr -> Maybe Addr
lookup x img = case img of
  ImgOfExpr e -> M.lookup e $ _addrOf x
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a $ _exprAt x

  ImgOfTplt is -> do
    mas <- ifNothings $ map (lookup x) is
    M.lookup (Tplt mas) $ _addrOf x

  ImgOfRel is i -> do
    mas <- ifNothings $ map (lookup x) is
    ma <- lookup x i
    M.lookup (Rel mas ma) $ _addrOf x

exprAt :: Rslt -> Addr -> Maybe Expr
exprAt = flip M.lookup . _exprAt

addrOf :: Rslt -> Expr -> Maybe Addr
addrOf = flip M.lookup . _addrOf

variety :: Rslt -> Addr -> Maybe (ExprCtr, Arity)
variety = flip M.lookup . _variety

-- | `has r a` finds the expression e at a in r, and returns
-- every position contained in e.
has :: Rslt -> Addr -> Maybe (Map Role Addr)
has r a = do exprAt r a
             maybe (Just M.empty) Just $ M.lookup a $ _has r

-- | `isIn r a` finds the expression e at a in r, and returns
-- every position that e occupies.
isIn :: Rslt -> Addr -> Maybe (Set (Role,Addr))
isIn r a = do
  exprAt r a
  maybe (Just S.empty) Just $ M.lookup a $ _isIn r

-- | `isIn1 r (role,a)` finds the expression that occupies
-- role in a.
isIn1 :: Rslt -> (Role, Addr) -> Maybe Addr
isIn1 x (r,a) = case M.lookup a $ _has x of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
