{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Input.IParse (pCommand) where

import           Data.Either.Combinators (mapLeft)
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hode.Hash.Convert
import Hode.Hash.HLookup
import Hode.Hash.HParse
import Hode.Hash.HTypes
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.UI.Types.Names
import Hode.Util.Misc
import Hode.Util.UParse


pCommand :: Rslt -> String -> Either String Command
pCommand r s =
  let (h,t) = splitAfterFirstLexeme s
  in case h of
    "/add"       -> pCommand_insert  r t
    "/a"         -> pCommand_insert  r t
    "/find"      -> pCommand_find    r t
    "/f"         -> pCommand_find    r t
    "/replace"   -> pCommand_replace r t
    "/r"         -> pCommand_replace r t
    "/delete"    -> pCommand_delete    t
    "/d"         -> pCommand_delete    t
    "/load"      -> pCommand_load      t
    "/save"      -> pCommand_save      t
    "/sortLeft"  -> pCommand_sort LeftFirst  r t
    "/sl"        -> pCommand_sort LeftFirst  r t
    "/sortRight" -> pCommand_sort RightFirst r t
    "/sr"        -> pCommand_sort RightFirst r t
    _            -> Left $ "Unrecognized start of command."

pCommand_insert :: Rslt -> String -> Either String Command
pCommand_insert r s = CommandInsert <$>
  prefixLeft "pCommand_insert"
  ( mapLeft show (parse _pHashExpr "UI.Input.IParse error 1" s)
    >>= pExprToHExpr r
    >>= hExprToExpr r )

pCommand_replace :: Rslt -> String -> Either String Command
pCommand_replace r s = prefixLeft "pCommand_replace" $ do
  (a,px) <- let p :: Parser (Addr, PExpr)
                p = do a <- fromIntegral <$> integer
                       px <- _pHashExpr
                       return (a,px)
    in mapLeft show $ parse p "UI.Input.IParse error 2" s
  e <- pExprToHExpr r px >>= hExprToExpr r
  Right $ CommandReplace a e

pCommand_delete :: String -> Either String Command
pCommand_delete s = prefixLeft "pCommand_delete" $ do
  a <- let p = fromIntegral <$> integer
       in mapLeft show $ parse p "UI.Input.IParse error 3" s
  Right $ CommandDelete a

-- | `pCommand_find` looks for any naked `/it` sub-expressions.
-- (Here naked means not inside an /eval expression.) If there are
-- any, the `PExpr` must be wrapped in a `PEval` constructor.
pCommand_find :: Rslt -> String -> Either String Command
-- PITFALL: Don't add an implicit Eval at the top of every search parsed in
-- the UI, because an Eval will return nothing if there are no Its below.
pCommand_find r s = prefixLeft "pCommand_find:" $ do
  (e1 :: PExpr) <- mapLeft show (parse _pHashExpr "UI.Input.IParse error 4" s)
  CommandFind s <$> pExprToHExpr r e1

-- TODO ? In `pCommand_sort`, it's kind of ugly that
-- `hExprToAddrs` is called for `ts` and not for `as`.
-- They could both be called downstream.
-- That would make it more like `pCommand_find`.
pCommand_sort :: BinOrientation -> Rslt -> String
              -> Either String Command

pCommand_sort bo r s =
  prefixLeft "pCommand_sort:" $ do
  let p :: Parser (PExpr,PExpr)
      p = do tplt   <- _pHashExpr
             search <- _pHashExpr
             return (tplt,search)
  (tplt,search) :: (PExpr,PExpr) <-
    mapLeft show $ parse p "UI.Input.IParse error 5" s
  ts :: Set TpltAddr <- pExprToHExpr r tplt >>=
                        hExprToAddrs r mempty
  h :: HExpr <- pExprToHExpr r search
  case S.toList ts of
    [t] -> Right $ CommandFindSort
           ("sort " ++ show bo ++ ": " ++ s) h bo t
    _ -> Left $ "Can only sort by exactly one Tplt, but "
         ++ " these Tplts were found: " ++ show ts ++ "."

pCommand_load :: String -> Either String Command
pCommand_load s = CommandLoad <$>
  ( prefixLeft "pCommand_load"
    $ mapLeft show (parse filepath "UI.Input.IParse error 6" s) )

pCommand_save :: String -> Either String Command
pCommand_save s = CommandSave <$>
  ( prefixLeft "pCommand_save"
    $ mapLeft show (parse filepath "UI.Input.IParse error 7" s) )
