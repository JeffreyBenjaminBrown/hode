-- | Minus mkRslt, these gory details are not
-- part of the Rslt interface.

{-# LANGUAGE
ScopedTypeVariables,
TupleSections,
LambdaCase
#-}

module Hode.Rslt.Index where

import           Control.Arrow (first)
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Data.Tuple

import           Hode.Rslt.RTypes
import           Hode.Rslt.RUtil


emptyRslt :: Rslt
emptyRslt = mkRslt mempty

mkRslt :: Map Addr RefExpr -> Rslt
mkRslt es = go es' where
  -- TODO ? speed: build ts at the same time as the map,
  -- to avoid looping over the addresses twice.
  ts :: Set Addr =
    M.keysSet $ M.filter f es where
    f = \case (Tplt' _) -> True
              _ -> False

  es' :: Map Addr RefExpr =
    if M.null es
    then M.fromList [ (0, Phrase' "")
                    , (1, Phrase' "transitive")
                    , (2, Phrase' "is")
                    , (3, Tplt' $ Tplt Nothing [2] Nothing)
                    , (4, Phrase' "sort by")
                    , (5, Phrase' "before")
                    , (6, Tplt' $ Tplt (Just 4) [] Nothing)
                    , (7, Tplt' $ Tplt (Just 4) [5] Nothing) ]
    else es

  go :: Map Addr RefExpr -> Rslt
  go m = let
    hasMap :: Map Addr (Map Role Addr) =
      M.filter (not . M.null) $
      M.map (M.fromList . refExprPositions) m
    in Rslt {
      _addrToRefExpr = m
    , _refExprToAddr = imgDb m
    , _variety = M.map refExprVariety m
    , _has = hasMap
    , _isIn = foldl invertAndAddPositions M.empty
              $ M.toList $ M.map M.toList hasMap
    , _tplts = ts
    }


-- | == Given an expression, look up an address.

imgDb :: Map Addr RefExpr -> Map RefExpr Addr
imgDb = M.fromList . catMaybes . map (Just . swap) . M.toList


-- | == Given an address, look up what it's connected to.
-- The following two functions are in a sense inverses.

-- | `refExprPositions e` gives every pair `(r,a)`
-- such that `a` plays the role `r` in `e`.

refExprPositions :: RefExpr -> [(Role,Addr)]
refExprPositions = \case
  Phrase' _          -> []
  Tplt' (Tplt a bs c) -> a' ++ bs' ++ c' where
    a' = maybeToList $ fmap (r,) a where
      r = RoleInTplt' RoleCapLeft
    bs' = map (first $ RoleInTplt' . RoleSeparator) $
          zip [1..] bs
    c' = maybeToList $ fmap (r,) c where
      r = RoleInTplt' RoleCapRight
  Rel'  (Rel mas ta) ->
    (RoleInRel' RoleTplt,ta) :
    map (first $ RoleInRel' . RoleMember) (zip [1..] mas)


-- | `invertAndAddPositions m (a, ras)` is meant for the case where m is a map
-- from addresses to the set of roles they play in other expressions, ras is
-- the set of roles in a, and a is not a key of m.
-- The map returned is like the input map, but extended by the information
-- in the pair.

invertAndAddPositions :: Map Addr (Set (Role, Addr))
                      -> (Addr,       [(Role, Addr)])
                      -> Map Addr (Set (Role, Addr))
invertAndAddPositions fm0 (a1, ras) = foldl f fm0 ras where
  f :: Map Addr (Set (Role, Addr))
    ->               (Role, Addr)
    -> Map Addr (Set (Role, Addr))
  f fm (r,a) = M.insertWith S.union a newData fm
    where newData :: Set (Role, Addr)
          newData = S.singleton (r,a1)
