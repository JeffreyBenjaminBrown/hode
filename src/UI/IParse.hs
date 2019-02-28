{-# LANGUAGE ScopedTypeVariables #-}

module UI.IParse where

import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hash.Convert
import Hash.HLookup
import Hash.HParse
import Hash.HTypes
import Qseq.QTypes
import Rslt.Edit
import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc
import Util.UParse


pCommand :: Rslt -> String -> Either String Command
pCommand r s =
  let (h,t) = splitAfterFirstLexeme s
  in case h of
    "/add"    -> pCommand_insert r t
    "/insert" -> pCommand_insert r t
    "/find"   -> pCommand_find t
    "/load"   -> pCommand_load t
    "/save"   -> pCommand_save t
    _         -> Left $ "pCommand: must start with "
                 ++ "/add, /insert, /find, /load or /save."

pCommand_insert :: Rslt -> String -> Either String Command
pCommand_insert r s = CommandInsert <$>
  ( prefixLeft "pCommand_insert"
    $ mapLeft show (parse pExpr "doh!" s)
    >>= pExprToHExpr
    >>= hExprToExpr r )

-- | `pCommand_find` looks for any naked `/it` sub-expressions.
-- (Here naked means not inside an /eval expression.) If there are
-- any, the `PExpr` must be wrapped in a `PEval` constructor.
pCommand_find :: String -> Either String Command
-- PITFALL: Don't add an implicit Eval at the top of every search parsed in
-- the UI, because an Eval will return nothing if there are no Its below.
pCommand_find s = do
  (e1 :: PExpr) <- prefixLeft "pCommand_find"
    $ mapLeft show (parse pExpr "doh!" s)
  let e2 = case pathsToIts_pExpr e1 of 
             [] -> e1
             _ -> PEval e1
  CommandFind <$> pExprToHExpr e2

pCommand_load :: String -> Either String Command
pCommand_load s = CommandLoad <$>
  ( prefixLeft "pCommand_load"
    $ mapLeft show (parse filepath "doh!" s) )

pCommand_save :: String -> Either String Command
pCommand_save s = CommandSave <$>
  ( prefixLeft "pCommand_save"
    $ mapLeft show (parse filepath "doh!" s) )


-- | = Functions from an `Rslt` and a parsed `String`,
-- to search, insert, show.
-- Theoretically, one could maintain an Rslt using GHCI with just these,
-- without ever using the TUI.

pInsert :: Rslt -> String -> Either String (Rslt, Addr)
pInsert r s = prefixLeft "pInsert"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToExpr r
  >>= exprToAddrInsert r

pFindAddrs :: Rslt -> String -> Either String (Set Addr)
pFindAddrs r s = prefixLeft "pFindAddrs"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToAddrs r (mempty :: Subst Addr)

pFindStrings :: Rslt -> String -> Either String (Set String)
pFindStrings r s = do
  (as :: Set Addr) <- prefixLeft "pFindExprs"
                      $ pFindAddrs r s
  (es :: Set Expr) <- ifLefts_set "pFindExprs"
                      $ S.map ( addrToExpr r ) as
  (ss :: Set String) <- ifLefts_set "pFindExprs"
                        $ S.map (eShow r) es
  return ss

pFindStringsIO :: Rslt -> String -> IO ()
pFindStringsIO r s =
  case (pFindStrings r s :: Either String (Set String))
  of Left err -> putStrLn err
     Right ss -> mapM_ putStrLn ss

