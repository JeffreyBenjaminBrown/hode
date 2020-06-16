module Hode.UI.Help (
  hodeHelp -- ^ Choice1Plist
  ) where

import qualified Data.List.PointedList as P
import           Data.Maybe

import           Hode.Brick.Help.Types
import qualified Hode.Hash.Parse.Keywords as KW
import           Hode.Hash.Parse.Util
import           Hode.UI.Input.KeyCmd
import           Hode.UI.Input.LangCmd.Parse


hodeHelp :: Choice1Plist
hodeHelp = let
  skippable :: String
  skippable = "(This has only one submode. Press space to skip.)"
  in fromJust $ P.fromList
  [ ( "universal key commands"
    , fromJust $ P.fromList [ (skippable, universal_c3) ] )
  , ( "ui text commands"
    , fromJust $ P.fromList [ (skippable, uiLangHelp) ] )

  , ( "the hash language"
    , fromJust $ P.fromList
      [ ("basic",                  hash_binops_c3)
      , ("subexpression matching", hash_subexp_c3)
      , ("transitivity",           hash_trans_c3)
      , ("unimportant",            hash_ignore_c3)
      ] )

  , ( "buffer view"
    , fromJust $ P.fromList [ (skippable, bufferBuffer_c3) ] )
  , ( "subgraph view"
    , fromJust $ P.fromList
      [ ("introduction", subgraphBuffer_universal_keyCmds_c3)
      , ("viewTree",     subgraphBuffer_primary_keyCmds_c3)
      , ("sort",         subgraphBuffer_sort_keyCmds_c3) ] ) ]

hash_binops_c3, hash_subexp_c3, hash_trans_c3, hash_ignore_c3 :: Choice3Plist
[ hash_binops_c3,
  hash_subexp_c3,
  hash_trans_c3,
  hash_ignore_c3 ] =

  map (fromJust . P.fromList . map hashKeyword_helpPair) $
  [ [ KW.hash
    , KW.hAnd
    , KW.hOr
    , KW.diff
    , KW.any
    , KW.tplt
    , KW.tplts
    , KW.addrs
    ]

  , [ KW.member
    , KW.involves
    , KW.eval
    , KW.it
    , KW.itMatching
    , KW.map
    ]

  , [ KW.reach
    , KW.transLeft
    , KW.transRight
    ]

  , [ KW.hashKeyword
    , KW.var ] ]
