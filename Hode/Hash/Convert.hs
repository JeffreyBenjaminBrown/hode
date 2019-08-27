-- | This module handles the step that follows parsing:
-- creating `HExpr`s from `PExpr`s and `PRel`s.

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Hash.Convert (
    pRelToHExpr          -- ^ PRel  -> Either String HExpr
  , pExprToHExpr         -- ^ PExpr -> Either String HExpr
  , pMapToHMap           -- ^ PMap  -> Either String HMap
  , pathsToIts_pExpr     -- ^ PExpr -> Either String [RolePath]
  , pathsToIts_sub_pExpr -- ^ PExpr -> Either String [RolePath]
  , pathsToIts_sub_pRel  -- ^ PRel  -> Either String [RolePath]
) where

import           Data.Functor.Foldable
import           Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow (second)
import Data.Functor (void)

import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | = Building an `HExpr` from a `PExpr`.

-- | In a PRel, outer *members* can be absent:
--
--   > parse pExpr "" "/hash x #j"
--   Right ( PRel ( Open 1 [ PNonRel $ PExpr $ Phrase "x"
--                         , Absent ]
--                  ["j"] ) )
--
-- In an HExpr, nothing is absent, but joints can be empty.
-- For every outer member of a PRel that is not Absent,
-- there should be an empty string added to that side of the
-- ExprTplt in the corresponding HExpr.

pRelToHExpr :: Rslt -> PRel -> Either String HExpr
pRelToHExpr r = prefixLeft "pRelToHExpr: " . para f where
  f :: Base PRel (PRel, Either String HExpr) -> Either String HExpr

  f (PNonRelF pn) = pExprToHExpr r pn -- PITFALL: must recurse by hand.
  f AbsentF = Left ", Absent represents no HExpr."
  f (OpenF _ ms js) = f $ ClosedF ms js

  f (ClosedF ms js0) = do
    let t = ExprTplt $ map Phrase js2 where
          absentLeft, absentRight :: Bool
          absentLeft  = case head ms of (Absent,_) -> True; _ -> False
          absentRight = case last ms of (Absent,_) -> True; _ -> False
          js1 = if not absentLeft  then "" : js0    else js0
          js2 = if not absentRight then js1 ++ [""] else js1

        ms' :: [(Role, (PRel, Either String HExpr))]
        ms' = let g :: PRel -> Bool
                  g Absent       = False
                  g (PNonRel px) = pExprIsSpecific px
                  g _            = True
          in filter (g . fst . snd)
             $ zip (map RoleMember [1..]) ms
        hms :: [(Role, Either String HExpr)]
        hms = map (second snd) ms'

    void $ ifLefts $ map snd hms
    let (hms' :: [(Role, HExpr)]) =
          map (second $ either (error "impossible") id) hms
    Right $ HMap $ M.insert RoleTplt (HExpr t)
      $ M.fromList hms'

-- | Using a recursion scheme for `pExprToHExpr` is hard.
-- c.f. the "WTF" comment below.
--
--  pExprToHExpr' :: PExpr -> Either String HExpr
--  pExprToHExpr' = para go where
--    pExprIsSpecificF :: Base PExpr (PExpr, Either String HExpr) -> Bool
--    pExprIsSpecificF = pExprIsSpecific . embed . fmap fst
--
--    go :: Base PExpr (PExpr, Either String HExpr) -> Either String HExpr
--    go p@(pExprIsSpecificF -> False) = Left $ "pExprToHExpr: " ++
--      show (embed $ fmap fst p) ++ " is not specific enough."
--    go p@(PExprF s) = Right $ HExpr s
--    go (PMapF s) = Right $ HMap $ ifLefts_map err $ fmap snd s
--      -- WTF?
--      where err = Left ""
--    go _ = error "todo: even more"

pExprToHExpr :: Rslt -> PExpr -> Either String HExpr
pExprToHExpr r0 pe0 = prefixLeft "-> pExprToHExpr" $ f r0 pe0 where
  f _ px@(pExprIsSpecific -> False) =
    Left $ show px ++ " is not specific enough."

  f _ (PExpr s)       = Right $ HExpr s
  f r (PMap m)        = HMap <$> pMapToHMap r m
  f r (PEval pnr)     = do (x :: HExpr) <- pExprToHExpr r pnr
                           ps <- pathsToIts_pExpr pnr
                           Right $ HEval x ps
  f _ (PVar s)        = Right $ HVar s
  f r (PDiff a b)     = do a' <- pExprToHExpr r a
                           b' <- pExprToHExpr r b
                           return $ HDiff a' b'
  f r (PAnd xs)       = do
    (l :: [HExpr]) <- ifLefts $ map (pExprToHExpr r) xs
    return $ HAnd l
  f r (POr xs)        = do
    (l :: [HExpr]) <- ifLefts $ map (pExprToHExpr r) xs
    return $ HOr l
  f r (PReach d pt ps) = do
    ht <- pExprToHExpr r pt
    hs <- pExprToHExpr r ps
    return $ HReach d ht hs
  f r (It (Just pnr)) = pExprToHExpr r pnr
  f r (PRel pr)       = pRelToHExpr r pr

  -- These redundant checks (to keep GHCI from warning me) should come last.
  f _ Any =
    Left $ "pExprToHExpr: Any is not specific enough."
  f _ (It Nothing) = Left
    $ "pExprToHExpr: It (Nothing) is not specific enough."


pMapToHMap :: Rslt -> PMap -> Either String HMap
pMapToHMap r = prefixLeft "-> pMapToHMap"
  . ifLefts_map
  . M.map (pExprToHExpr r)
  . M.filter pExprIsSpecific


-- | = Finding the `It`s for a `PEval` to evaluate.

pathsToIts_pExpr :: PExpr -> Either String [RolePath]
pathsToIts_pExpr (PEval pnr) = pathsToIts_sub_pExpr pnr
pathsToIts_pExpr x           = pathsToIts_sub_pExpr x

pathsToIts_sub_pExpr :: PExpr -> Either String [RolePath]
pathsToIts_sub_pExpr = prefixLeft "-> pathsToIts_sub_pExpr" . para f where
  tooLate :: Base PExpr (PExpr, Either String [RolePath])
           -> Either String [RolePath]
  tooLate x = Left $ "pathsToIts_sub_pExpr called too late (too far leafward in the PExpr), on " ++ show (embed $ fmap fst x)

  f :: Base PExpr (PExpr, Either String [RolePath])
    -> Either String [RolePath]
  f (PExprF _) = Right []
  f (PMapF m)  = do (m' :: Map Role [RolePath]) <-
                      ifLefts_map $ M.map snd m
                    let g :: (Role, [RolePath]) -> [RolePath]
                        g (role, paths) = map ((:) role) paths
                    Right $ concatMap g $ M.toList m'
  f (PEvalF _) = Right []
    -- don't recurse into a new PEval context; the paths to
    -- that PEval's `it`s are not the path to this one's.
  f (PVarF _)        = Right []
  f x@(PDiffF _ _)    = tooLate x
  f x@(PAndF _)       = tooLate x
  f x@(POrF _)        = tooLate x
  f x@(PReachF _ _ _) = tooLate x
  f AnyF             = Right []
  f (ItF Nothing)    = Right [[]]
  f (ItF (Just pnr)) = fmap ([] :) $ snd pnr
  f (PRelF pr)       = pathsToIts_sub_pRel pr

pathsToIts_sub_pRel :: PRel -> Either String [RolePath]
pathsToIts_sub_pRel = prefixLeft "-> pathsToIts_sub_pRel" . cata f where
  f :: Base PRel (Either String [RolePath])
    -> Either String [RolePath]
  f AbsentF         = Right []
  f (PNonRelF pnr)  = pathsToIts_sub_pExpr pnr
  f (OpenF _ ms js) = f $ ClosedF ms js
  f (ClosedF ms _)  = do
    let g :: (Int,[RolePath]) -> [RolePath]
        g (i,ps) = map ((:) $ RoleMember i) ps
    ms' <- ifLefts ms
    Right $ concatMap g $ zip [1..] ms'
