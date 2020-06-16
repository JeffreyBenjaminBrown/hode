module Hode.UI.Help (
  hodeHelp -- ^ Choice1Plist
  ) where

import qualified Data.List.PointedList as P
import           Data.Maybe

import Hode.Brick.Help.Types
import Hode.UI.Input.KeyCmd
import Hode.UI.Input.LangCmd.Parse


hodeHelp :: Choice1Plist
hodeHelp = let
  skippable :: String
  skippable = "(This has only one submode. Press space to skip.)"
  in fromJust $ P.fromList
  [ ( "ui text commands"
    , fromJust $ P.fromList [ (skippable, uiLangHelp) ] )
  , ( "universal key commands"
    , fromJust $ P.fromList [ (skippable, universal_c3) ] )
  , ( "buffer view"
    , fromJust $ P.fromList [ (skippable, bufferBuffer_c3) ] )
  , ( "subgraph view"
    , fromJust $ P.fromList
      [ ("introduction", subgraphBuffer_universal_keyCmds_c3)
      , ("viewTree",     subgraphBuffer_primary_keyCmds_c3)
      , ("sort",         subgraphBuffer_sort_keyCmds_c3) ] ) ]
