-- | = Functions from an `Rslt` and a parsed `String`,
-- to search, insert, show.
-- Theoretically, one could maintain an Rslt using GHCI with just these,
-- without ever using the TUI.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.NoUI (
  -- | = Display the entire graph.
  -- (These are useful when you're learning the app.
  -- Once your graph is big, they won't be practical.)
    nShowRsltRefExprs    -- ^ Rslt -> [String]
  , nShowRsltRefExprsIO  -- ^ Rslt -> IO ()
  , nShowRslt   -- ^ Rslt -> Either String [String]
  , nShowRsltIO -- ^ Rslt -> IO ()

  -- | = edit: insert
  , nInsert        -- ^ Rslt -> String -> Either String (Rslt, Addr)
  , nInsert'       -- ^ Rslt -> String -> Either String Rslt
  , nInserts       -- ^ Foldable f =>
                   --   Rslt -> f String -> Either String Rslt

  -- | = edit: other
  , nDelete        -- ^ Rslt -> String         -> Either String Rslt
  , nReplace       -- ^ Rslt -> Addr -> String -> Either String Rslt
  , nReplaceInRole -- ^ Rslt -> Role -> Addr -> String ->
                   --   Either String Rslt

  -- | = search (and display)
  , nFind            -- ^ Rslt
                     -- -> String -> Either String [(Addr, Expr)]
  , nFindSort        -- ^ Rslt -> (BinOrientation, TpltAddr)
                     -- -> String -> Either String [(Addr, Expr)]
  , nFindStrings     -- ^ Rslt
                     -- -> String -> Either String [(Addr, String)]
  , nFindSortStrings -- ^ Rslt -> (BinOrientation, TpltAddr)
                     -- -> String -> Either String [(Addr, String)]
  , nFindIO     -- ^ Rslt                              -> String -> IO ()
  , nFindSortIO -- ^ Rslt -> (BinOrientation,TpltAddr) -> String -> IO ()

  , module Hode.NoUI.Internal
  ) where

import           Control.Monad (foldM)
import qualified Data.Map as M
import qualified Data.Set as S

import Hode.NoUI.Internal
import Hode.Rslt.Binary
import Hode.Rslt.Edit
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.Sort
import Hode.Util.Misc


-- | = Display the entire graph.
-- (These are useful when you're learning the app.
-- Once your graph is big, they won't be practical.)

nShowRsltRefExprs :: Rslt -> [String]
nShowRsltRefExprs r = let
  m = _addrToRefExpr r
  showPair k = let
    val = m M.! k
    in show k ++ ": " ++ show val
  in fmap showPair $ M.keys m

nShowRsltRefExprsIO :: Rslt -> IO ()
nShowRsltRefExprsIO = mapM_ putStrLn . nShowRsltRefExprs

nShowRslt :: Rslt -> Either String [String]
nShowRslt r = do
  let as :: [Addr] = M.keys $ _addrToRefExpr r
  es <- ifLefts $ map (addrToExpr r) as
  ss <- ifLefts $ map (eShow r) es
  let f :: (Addr,String) -> String
      f (a,s) = show a ++ ": " ++ s
  Right $ map f $ zip as ss

nShowRsltIO :: Rslt -> IO ()
nShowRsltIO r = either putStrLn (mapM_ putStrLn) $
                nShowRslt r


-- | = edit: insert

nInsert :: Rslt -> String -> Either String (Rslt, [Aged Addr])
nInsert r s = prefixLeft "nInsert: " $
              nExpr r s >>= exprToAddrInsert r

nInsert' :: Rslt -> String -> Either String Rslt
nInsert' r s = fst <$> nInsert r s

nInserts :: Foldable f
         => Rslt -> f String -> Either String Rslt
nInserts r ss = foldM nInsert' r ss


-- | = edit: other

nDelete :: Rslt -> String -> Either String Rslt
nDelete r s = prefixLeft "nDelete: " $
              nExpr r s >>= exprToAddr r >>= flip delete r

nReplace :: Rslt -> Addr -> String -> Either String Rslt
nReplace r a s =
  prefixLeft "nReplace: " $ do e <- nExpr r s
                               replaceExpr a e r

nReplaceInRole :: Rslt -> Role -> Addr -> String
               -> Either String Rslt
nReplaceInRole r role host new =
  prefixLeft "nReplaceInRole: " $ do
  new' <- nExpr r new >>= exprToAddr r
  replaceInRole role new' host r


-- | = search (and display)

nFind :: Rslt -> String -> Either String [(Addr, Expr)]
nFind r s = do as <- S.toList <$> nFindAddrs r s
               es <- ifLefts $ map (addrToExpr r) as
               Right $ zip as es

nFindSort :: Rslt -> (BinOrientation, TpltAddr)
          -> String -> Either String [(Addr, Expr)]
nFindSort r bt s = do
  as :: [Addr] <- S.toList <$> nFindAddrs r s
                  >>= kahnSort r bt
  es :: [Expr] <- ifLefts $ map (addrToExpr r) as
  Right $ zip as es

nFindStrings :: Rslt -> String -> Either String [(Addr, String)]
nFindStrings r s = do
  (as,es) :: ([Addr],[Expr]) <- unzip <$> nFind r s
  ss <- ifLefts $ map (eShow r) es
  Right $ zip as ss

nFindSortStrings :: Rslt -> (BinOrientation, TpltAddr)
                 -> String -> Either String [(Addr, String)]
nFindSortStrings r bt s = do
  (as,es) :: ([Addr],[Expr]) <- unzip <$> nFindSort r bt s
  ss <- ifLefts $ map (eShow r) es
  Right $ zip as ss

nFindIO :: Rslt -> String -> IO ()
nFindIO r q =
  case nFindStrings r q
  of Left err -> putStrLn err
     Right ss -> let f (a,s) = show a ++ ": " ++ s
                 in mapM_ putStrLn $ map f ss

nFindSortIO :: Rslt -> (BinOrientation,TpltAddr) -> String -> IO ()
nFindSortIO r bt q =
  case nFindSortStrings r bt q
  of Left err -> putStrLn err
     Right ss -> let f (a,s) = show a ++ ": " ++ s
                 in mapM_ putStrLn $ map f ss

