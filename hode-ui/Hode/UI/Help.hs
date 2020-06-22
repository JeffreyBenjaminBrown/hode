module Hode.UI.Help (
  hodeHelp -- ^ Choice1Plist
  ) where

import           Control.Lens
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
  skippable = "(This has only one submode. Press space to skip this choice.)"
  in fromJust $ P.fromList

  [ ( "universal key commands"
    , fromJust $ P.fromList
      [ ("change mode",    change_mode_keyCmds_c3)
      , ("change submode", change_submode_keyCmds_c3) ] )

  , ( "graph buffer"
    , fromJust $ P.fromList
      [ ("fundamentals", graphBuffer_universal_keyCmds_c3)
      , ("viewTree",     graphBuffer_primary_keyCmds_c3)
      , ("order",        graphBuffer_sort_keyCmds_c3) ] )

  , ( "ui text commands"
    , fromJust $ P.fromList
      [ ("fundamentals", uiLangHelp_basic)
      , ("order",        uiLangHelp_sort) ] )

  , ( "the hash language"
    , fromJust $ P.fromList
      [ ("fundamentals",           hash_fundamentals_c3)
      , ("subexpression matching", hash_subexp_c3)
      , ("order & transitivity",   hash_trans_c3)
      , ("unimportant",            hash_ignore_c3)
      ] )

  , ( "`select graph buffer` buffer"
    , fromJust $ P.fromList [ (skippable, bufferBuffer_c3) ] )
  ]

hash_fundamentals_c3, hash_subexp_c3, hash_trans_c3, hash_ignore_c3 :: Choice3Plist
[ hash_fundamentals_c3,
  hash_subexp_c3,
  hash_trans_c3,
  hash_ignore_c3 ] =

  let ss :: HashKeyword -> (String, String)
      ss hk = _2 %~ synonymBlurb hk $
              hashKeyword_helpPair hk

  in fromJust . P.fromList . map ss <$>
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
