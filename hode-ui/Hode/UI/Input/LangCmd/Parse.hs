{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.Input.LangCmd.Parse (
    pLangCmd -- ^ Rslt -> String -> Either String LangCmd
  , uiLangHelp_basic -- ^ Choice3Plist
  , uiLangHelp_sort  -- ^ Choice3Plist
  ) where

import           Data.Either.Combinators (mapLeft)
import           Data.List (maximumBy)
import qualified Data.List.PointedList as P
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hode.Brick.Help.Types
import Hode.Hash.Convert
import Hode.Hash.Lookup
import Hode.Hash.Parse
import Hode.Hash.Types
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.UI.Types.Names
import Hode.Util.Misc
import Hode.Util.Parse


data LangCmd_MapItem = LangCmd_MapItem
  { _langCmd_name  :: String
  , _langCmd_func  :: Rslt -> String -> Either String LangCmd
  , _langCmd_keywords :: [String]
  , _langCmd_guide :: String }

uiLangHelp_basic, uiLangHelp_sort :: Choice3Plist
[ uiLangHelp_basic,
  uiLangHelp_sort] =

  let
    prefix_synonymBlurb :: LangCmd_MapItem -> (String -> String)
    prefix_synonymBlurb lm =
      case _langCmd_keywords lm of
        []  -> error $ _langCmd_name lm ++ " has no associated keywords."
        [_] -> id
        as  -> \s -> "(The keywords {"
          ++ concatMap (++ " and ") (tail as) ++ head as
          ++ "} are synonyms. They do exactly the same thing, "
          ++ "and id doesn't matter which you use.)\n\n"
          ++ s

    helpItem :: LangCmd_MapItem -> (String, String)
    helpItem i =
      let kw :: String =
            maximumBy (comparing length) $ _langCmd_keywords i
      in ( kw ++ ": " ++ _langCmd_name i
         , prefix_synonymBlurb i $ _langCmd_guide i )

  in fromJust . P.fromList . map (helpItem)
     <$>
     [ langCmd_MapItems_basic
     , langCmd_MapItems_sort ]

langCmd_MapItems_basic :: [LangCmd_MapItem]
langCmd_MapItems_basic =
  [ LangCmd_MapItem {
      _langCmd_name = "add expression",
      _langCmd_func = pLangCmd_insert,
      _langCmd_keywords = ["/add","/a"],
      _langCmd_guide = paragraphs
        [ "Adds an expression to the graph."
        , "For instance, `/add Bob #likes pizza`." ] }

  , LangCmd_MapItem {
      _langCmd_name = "find",
      _langCmd_func = pLangCmd_find,
      _langCmd_keywords = ["/find","/f"],
      _langCmd_guide = paragraph
        [ "Find an expression or set of expressions in the graph."
        , "For instance, `/find /_ #likes pizza` would find both `Bob #likes pizza` and `Mary #likes pizza`." ] }

  , LangCmd_MapItem {
      _langCmd_name = "replace",
      _langCmd_func = pLangCmd_replace,
      _langCmd_keywords = ["/replace","/r"],
      _langCmd_guide = paragraph
        [ "Replace the content of an expression."
        , "The first argument is the address, and the second is the expression to replace it with."
        , "For instance, `/replace 100 Bob #needs damp sponges`." ] }

  , LangCmd_MapItem {
      _langCmd_name = "move",
      _langCmd_func = \_ t -> pLangCmd_move t,
      _langCmd_keywords = ["/move","/m"],
      _langCmd_guide = paragraph
        [ "Move an expression to a new address."
        , "The first argument is the old address, and the second one is the new address."
        , "For instance, `/move 33 333` would move whatever was at address 33 to address 333."
        , "PITFALL: If the new address is not empty, it will be moved to an unoccupied address -- specifically, the one just after the maximum address in the graph." ] }

  , LangCmd_MapItem {
      _langCmd_name = "delete",
      _langCmd_func = \_ t -> pLangCmd_delete t,
      _langCmd_keywords = ["/delete","/d"],
      _langCmd_guide = paragraph
        [ "Deletes the expression at the given address."
        , "For instance, `/d 77` deletes what was at address 77."
        , "PITFALL: An expression can only be deleted if it is not a member of any other expression." ] }

  , LangCmd_MapItem {
      _langCmd_name = "load",
      _langCmd_func = \_ t -> pLangCmd_load t,
      _langCmd_keywords = ["/load","/l"],
      _langCmd_guide = paragraph
        [ "Loads the data at the specified path."
        , "For instance, `/load path/to/my/data`."
        , "Every file ending in `.rslt` is loaded."
        , "All others are ignored." ] }

  , LangCmd_MapItem {
      _langCmd_name = "save",
      _langCmd_func = \_ t -> pLangCmd_save t,
      _langCmd_keywords = ["/save","/s"],
      _langCmd_guide = paragraph
        [ "Saves the data at the specified path."
        , "For instance, `/load path/to/my/data`."
        , "PITFALL: Does not delete anything that was previously there."
        , "If there was already a graph there, this can create an inconsistent graph -- it might have duplicate entries, and if so you'll only find one of them when you search for it."
        , "To be safe, first delete all files ending in `.rslt` from the destination." ] }
  ]

langCmd_MapItems_sort :: [LangCmd_MapItem]
langCmd_MapItems_sort =
  [ LangCmd_MapItem {
      _langCmd_name = "sort right",
      _langCmd_func = pLangCmd_sort RightEarlier,
      _langCmd_keywords = ["/sortRight","/sr"],
      _langCmd_guide = paragraph
        [ "Sorts by the given template, putting right members earlier, left members later."
        , "Example: `/sortRight /@ 14` sorts by the template located at address 14, and `/sortLeft (/t /_ eat /_)` sorts by the `_ eat _` template."
        , "If the template is t, and two expressions a and b are related by t, then b will come first in the sort order, because b is on the right side of the template."
        , "For details on sorting, see docs/ui/transitivity.md in your clone of Hode, or visit https://github.com/JeffreyBenjaminBrown/hode/blob/master/docs/ui/transitivity.md." ] }

  , LangCmd_MapItem {
      _langCmd_name = "sort left",
      _langCmd_func = pLangCmd_sort LeftEarlier,
      _langCmd_keywords = ["/sortLeft","/sl"],
      _langCmd_guide = paragraph
        [ "Sorts by the given template, putting left members earlier, right members later."
        , "Example: `/sortLeft /@ 14` sorts by the template located at address 14, and `/sortLeft (/t /_ eat /_)` sorts by the `_ eat _` template."
        , "If the template is t, and two expressions a and b are related by t, then a will come first in the sort order, because b is on the left side of the template."
        , "For details on sorting, see docs/ui/transitivity.md in your clone of Hode, or visit https://github.com/JeffreyBenjaminBrown/hode/blob/master/docs/ui/transitivity.md." ] }
  ]

pLangCmd :: Rslt -> String -> Either String LangCmd
pLangCmd r s =
  let (h,t) = splitAfterFirstLexeme s
      langCmd_map :: Map String (Rslt -> String -> Either String LangCmd)
      langCmd_map = M.fromList $ concatMap f langCmd_MapItems_basic where
        f lcmi = [ ( x
                   , _langCmd_func lcmi )
                 | x <- _langCmd_keywords lcmi ]
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
