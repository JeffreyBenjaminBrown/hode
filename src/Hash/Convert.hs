{-# LANGUAGE PartialTypeSignatures #-}
-- | After parsing, the next step is to
-- create `HExpr`s from `PExpr`s and `PRel`s.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.Convert (
    pRelToHExpr      -- PRel  -> Either String HExpr
  , pExprToHExpr     -- PExpr -> Either String HExpr
  , pMapToHMap       -- PMap  -> Either String HMap
  , pathsToIts_pExpr -- PExpr -> [RolePath]
) where

import           Data.Functor.Foldable
import qualified Data.Map       as M

import Control.Arrow (second)
import Data.Functor (void)
import Lens.Micro

import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes
import Util.Misc


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

pRelToHExpr :: PRel -> Either String HExpr
pRelToHExpr = para f where
  f :: Base PRel (PRel, Either String HExpr) -> Either String HExpr

  f (PNonRelF pn) = pExprToHExpr pn -- PITFALL: must recurse by hand.
  f AbsentF = Left "pRelToHExpr: Absent represents no HExpr."
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

    void $ ifLefts "pRelToHExpr" $ map snd hms
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
--   go (PMapF s) = Right $ HMap $ ifLefts_map err $ fmap snd s
--      -- WTF?
--      where err = Left ""
--    go _ = error "todo: even more"

pExprToHExpr :: PExpr -> Either String HExpr
pExprToHExpr px@(pExprIsSpecific -> False) = Left
  $ "pExprToHExpr: " ++ show px ++ " is not specific enough."

pExprToHExpr (PExpr s)       = Right $ HExpr s
pExprToHExpr (PMap m)        = HMap <$> pMapToHMap m
pExprToHExpr (PEval pnr)     = do
  (x :: HExpr)  <- pExprToHExpr pnr
  Right $ HEval x $ pathsToIts_pExpr pnr
pExprToHExpr (PVar s)        = Right $ HVar s
pExprToHExpr (PDiff a b)     = do a' <- pExprToHExpr a
                                  b' <- pExprToHExpr b
                                  return $ HDiff a' b'
pExprToHExpr (PAnd xs)       = do
  (l :: [HExpr]) <- ifLefts "pExprToHExpr" $ map pExprToHExpr xs
  return $ HAnd l
pExprToHExpr (POr xs)       = do
  (l :: [HExpr]) <- ifLefts "pExprToHExpr" $ map pExprToHExpr xs
  return $ HOr l
pExprToHExpr (It (Just pnr))        = pExprToHExpr pnr
pExprToHExpr (PRel pr)              = pRelToHExpr pr
pExprToHExpr (PPar (Par spPairs s)) =
  let spPairs' = map (_2 %~ pExprToHExpr) spPairs
  in error "HExpr cannot yet accommodate paragraphs."

-- These redundant checks (to keep GHCI from warning me) should come last.
pExprToHExpr Any =
  Left $ "pExprToHExpr: Any is not specific enough."
pExprToHExpr (It Nothing) = Left
  $ "pExprToHExpr: It (Nothing) is not specific enough."


pMapToHMap :: PMap -> Either String HMap
pMapToHMap = ifLefts_map "pMapToHMap"
  . M.map pExprToHExpr
  . M.filter pExprIsSpecific


-- | = Finding the `It`s for a `PEval` to evaluate.

pathsToIts_pExpr :: PExpr -> [RolePath]
pathsToIts_pExpr (PEval pnr) = pathsToIts_sub_pExpr pnr
pathsToIts_pExpr x           = pathsToIts_sub_pExpr x

pathsToIts_sub_pExpr :: PExpr -> [RolePath]
pathsToIts_sub_pExpr = para f where
  err :: Base PExpr (PExpr, [RolePath]) -> [RolePath]
  err x = error $ "pathsToIts_sub_pExpr called too late, on "
          ++ show (embed $ fmap fst x)

  f :: Base PExpr (PExpr, [RolePath]) -> [RolePath]
  f (PExprF _)       = []
  f (PMapF m)        = concatMap g $ M.toList $ M.map snd m
    where g (role, paths) = map ((:) role) paths
  f (PEvalF _)       = []
    -- don't recurse into a new PEval context; the paths to
    -- that PEval's `it`s are not the path to this one's.
  f (PVarF _)        = []
  f x@(PDiffF _ _)   = err x
  f x@(PAndF _)      = err x
  f x@(POrF _)       = err x
  f AnyF             = []
  f (ItF Nothing)    = [[]]
  f (ItF (Just pnr)) = [] : snd pnr
  f x@(PParF _)      = err x
  f (PRelF pr)       = pathsToIts_sub_pRel pr

pathsToIts_sub_pRel :: PRel -> [RolePath]
pathsToIts_sub_pRel = cata f where
  f :: Base PRel [RolePath] -> [RolePath]
  f AbsentF         = []
  f (PNonRelF pnr)  = pathsToIts_sub_pExpr pnr
  f (OpenF _ ms js) = f $ ClosedF ms js
  f (ClosedF ms _)  = concatMap g $ zip [1..] ms where
    g :: (Int,[RolePath]) -> [RolePath]
    g (i,ps) = map ((:) $ RoleMember i) ps
