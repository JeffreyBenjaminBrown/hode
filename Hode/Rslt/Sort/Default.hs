-- | The user can use the built-in `Tplt`s "sort by _" (unary)
-- and "sort by _ before _" (binary) to state which `Tplt`s
-- should be used by default to order the data on-screen.

{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Rslt.Sort.Default (
    firstApplicableTplt -- ^ Rslt -> [Addr] -> Either String (Maybe TpltAddr)
  , isIn_usingTplt      -- ^ Rslt -> TpltAddr -> Addr -> Either String Bool
  , usesTplt            -- ^ Rslt -> TpltAddr -> Addr -> Bool
  , sortTpltsForSorting -- ^ Rslt -> Either String [TpltAddr]
  ) where

import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.Binary
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.Sort
import Hode.Util.Misc


firstApplicableTplt :: Rslt -> [Addr] -> Either String (Maybe TpltAddr)
-- TODO ? It would be better if this stopped after finding two `Addr`s,
-- rather than just one, that are in `Rel`s involving the `Tplt`,
-- because when only one of the `Addr`s is in a `t`-`Rel`,
-- `t` has nothing useful to say about the order of the `Addr`s.
firstApplicableTplt r as =
  sortTpltsForSorting r >>= go
  where
  go :: [TpltAddr] -> Either String (Maybe TpltAddr)
  go [] = Right Nothing
  go (t:ts) = do
    bs :: [Bool] <- ifLefts $ map (isIn_usingTplt r t) as
    case or bs of True -> Right $ Just t
                  False -> go ts

-- TODO ? speed: This currently is run every time sorting is needed.
-- If either "sort by _" or "sort by _ before _" becomes large
-- (which seems dubious), it will be desirable to store the result.

sortTpltsForSorting :: Rslt -> Either String [TpltAddr]
sortTpltsForSorting r = do
  sb    ::     Addr <- head . S.toList <$> -- safe b/c it's in every `Rslt`
    ( hExprToAddrs r mempty $ HExpr $ ExprTplt $ Tplt
      (Just $ Phrase "sort by") []                Nothing)
  sb_b4 ::     Addr <- head . S.toList <$> -- safe b/c it's in every `Rslt`
    ( hExprToAddrs r mempty $ HExpr $ ExprTplt $ Tplt
      (Just $ Phrase "sort by") [Phrase "before"] Nothing)
  ts    :: Set Addr <-
    hExprToAddrs r mempty $ HAnd
    [ HTplts -- templates
    , HOr [
        HEval  -- things to sort by
            (HMap $ M.singleton (RoleInRel' RoleTplt) $ HExpr $ ExprAddr sb)
            [[RoleMember 1]]
          , HEval  -- things to sort by before or after other things
            (HMap $ M.singleton (RoleInRel' RoleTplt) $ HExpr $ ExprAddr sb_b4)
            [[RoleMember 1],[RoleMember 2]] ]]
  (sorted, isol) <-
    kahnSort r (LeftFirst, sb_b4) $ S.toList ts
  Left ( -- for debugging
    "ts: " ++ show ts ++ "\nsorted: " ++ show sorted
      ++ "\nisol: " ++ show isol )
  Right $ sorted ++ isol

-- | `isIn_usingTplt r t a` determines whether `a` is in a "`t`-`Rel`":
-- a `Rel` in which `t` is the `Tplt`.
-- This is faster than the `HExpr` method,
-- and needfully so, since `Tplt`s can have high order.
isIn_usingTplt :: Rslt -> TpltAddr -> Addr -> Either String Bool
isIn_usingTplt r t a =
  or . S.map (usesTplt r t . snd)
  <$> isIn r a

-- | `usesTplt r t0 a` determines whether `a` is a "`t`-`Rel`":
-- a `Rel` in which `t` is the `Tplt`.
-- This is faster than the `HExpr` method,
-- and needfully so, since `Tplt`s can have high order.
usesTplt :: Rslt -> TpltAddr -> Addr -> Bool
usesTplt r t0 a =
  case M.lookup a $ _addrToRefExpr r of
    Just (Rel' (Rel _ t)) -> t == t0
    _                     -> False
