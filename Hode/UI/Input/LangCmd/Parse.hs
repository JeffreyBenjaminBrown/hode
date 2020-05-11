{-# LANGUAGE ScopedTypeVariables
, TemplateHaskell
#-}

module Hode.UI.Input.LangCmd.Parse (pLangCmd) where

import           Control.Lens
import           Data.Either.Combinators (mapLeft)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hode.Hash.Convert
import Hode.Hash.Lookup
import Hode.Hash.Parse
import Hode.Hash.Types
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.Util.Misc
import Hode.Util.Parse


data LangCmd_MapItem = LangCmd_MapItem
  { _langCmd_name  :: String
  , _langCmd_func  :: Rslt -> String -> Either String LangCmd
  , _langCmd_keyword :: [String]
  , _langCmd_guide :: String }
makeLenses ''LangCmd_MapItem

langCmds =
  [ LangCmd_MapItem {
      _langCmd_name = "add expression",
      _langCmd_func = pLangCmd_insert,
      _langCmd_keyword = ["/add","/a"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "find",
      _langCmd_func = pLangCmd_find,
      _langCmd_keyword = ["/find","/f"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "replace",
      _langCmd_func = pLangCmd_replace,
      _langCmd_keyword = ["/replace","/r"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "move",
      _langCmd_func = \_ t -> pLangCmd_move t,
      _langCmd_keyword = ["/move","/m"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "delete",
      _langCmd_func = \_ t -> pLangCmd_delete t,
      _langCmd_keyword = ["/delete","/d"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "load",
      _langCmd_func = \_ t -> pLangCmd_load t,
      _langCmd_keyword = ["/load","/l"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "save",
      _langCmd_func = \_ t -> pLangCmd_save t,
      _langCmd_keyword = ["/save","/s"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "sort right",
      _langCmd_func = pLangCmd_sort RightEarlier,
      _langCmd_keyword = ["/sortRight","/sr"],
      _langCmd_guide = "describe" }

  , LangCmd_MapItem {
      _langCmd_name = "sort left",
      _langCmd_func = pLangCmd_sort LeftEarlier,
      _langCmd_keyword = ["/sortLeft","/sl"],
      _langCmd_guide = "describe" }
  ]

pLangCmd :: Rslt -> String -> Either String LangCmd
pLangCmd r s =
  let (h,t) = splitAfterFirstLexeme s
      langCmd_map :: Map String (Rslt -> String -> Either String LangCmd)
      langCmd_map = M.fromList $ concatMap f langCmds where
        f lcmi = [ ( x
                   , _langCmd_func lcmi )
                 | x <- _langCmd_keyword lcmi ]
  in case M.lookup h langCmd_map of
    Just f -> f r t
    _      -> Left $ "Unrecognized start of command."

pLangCmd_insert :: Rslt -> String -> Either String LangCmd
pLangCmd_insert r s = LangCmdInsert <$>
  prefixLeft "pLangCmd_insert:"
  ( mapLeft show (parse _pHashExpr "UI.Input.Parse error 1" s)
    >>= pExprToHExpr r
    >>= hExprToExpr r )

pLangCmd_replace :: Rslt -> String -> Either String LangCmd
pLangCmd_replace r s = prefixLeft "pLangCmd_replace:" $ do
  (a,px) <- let p :: Parser (Addr, PExpr)
                p = do a <- fromIntegral <$> integer
                       px <- _pHashExpr
                       return (a,px)
    in mapLeft show $ parse p "UI.Input.Parse error 2" s
  e <- pExprToHExpr r px >>= hExprToExpr r
  Right $ LangCmdReplace a e

pLangCmd_move :: String -> Either String LangCmd
pLangCmd_move s = prefixLeft "pLangCmd_move:" $ do
  (old,new) <- let p :: Parser (Addr, Addr)
                   p = do old <- fromIntegral <$> integer
                          new <- fromIntegral <$> integer
                          return (old,new)
    in mapLeft show $ parse p "UI.Input.Parse error 2.5" s
  Right $ LangCmdMove old new

pLangCmd_delete :: String -> Either String LangCmd
pLangCmd_delete s = prefixLeft "pLangCmd_delete:" $ do
  a <- let p = fromIntegral <$> integer
       in mapLeft show $ parse p "UI.Input.Parse error 3" s
  Right $ LangCmdDelete a

-- | `pLangCmd_find` looks for any naked `/it` sub-expressions.
-- (Here naked means not inside an /eval expression.) If there are
-- any, the `PExpr` must be wrapped in a `PEval` constructor.
pLangCmd_find :: Rslt -> String -> Either String LangCmd
-- PITFALL: Don't add an implicit Eval at the top of every search parsed in
-- the UI, because an Eval will return nothing if there are no Its below.
pLangCmd_find r s = prefixLeft "pLangCmd_find:" $ do
  e1 :: PExpr <- mapLeft show (parse _pHashExpr "UI.Input.Parse error 4" s)
  LangCmdFind s <$> pExprToHExpr r e1

-- TODO ? In `pLangCmd_sort`, it's kind of ugly that
-- `hExprToAddrs` is called for `ts` and not for `as`.
-- They could both be called downstream.
-- That would make it more like `pLangCmd_find`.
pLangCmd_sort :: BinOrientation -> Rslt -> String
              -> Either String LangCmd

pLangCmd_sort bo r s =
  prefixLeft "pLangCmd_sort:" $ do
  tplt :: PExpr <- mapLeft show $
    parse _pHashExpr "UI.Input.Parse error 5" s
  ts :: Set TpltAddr <- pExprToHExpr r tplt >>=
                        hExprToAddrs r mempty
  case S.toList ts of
    [t] -> Right $ LangCmdSort
           ("sort " ++ show bo ++ ": " ++ s) bo t
    _ -> Left $ "Can only sort by exactly one Tplt, but "
         ++ " these Tplts were found: " ++ show ts ++ "."

pLangCmd_load :: String -> Either String LangCmd
pLangCmd_load s = LangCmdLoad <$>
  ( prefixLeft "pLangCmd_load:"
    $ mapLeft show (parse filepath "UI.Input.Parse error 6" s) )

pLangCmd_save :: String -> Either String LangCmd
pLangCmd_save s = LangCmdSave <$>
  ( prefixLeft "pLangCmd_save:"
    $ mapLeft show (parse filepath "UI.Input.Parse error 7" s) )
