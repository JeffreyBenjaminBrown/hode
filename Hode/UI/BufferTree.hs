{-# LANGUAGE
ScopedTypeVariables,
ViewPatterns
#-}

module Hode.UI.BufferTree
  ( insert_focusedViewExpr_asChildOfBuffer
                            -- ^ St -> Either String St
  , insertBuffer_topNext      -- ^ Buffer    -> St -> St
  , insertBuffer_asChild      -- ^ Buffer    -> St -> St

  , deleteFocused_buffer    -- ^ St -> St

  , nudgeFocus_inBufferTree -- ^ Direction -> St -> St
  , nudgeFocused_buffer     -- ^ Direction -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Util
import Hode.UI.Util.String
import Hode.UI.Window
import Hode.Util.Misc
import Hode.PTree


insert_focusedViewExpr_asChildOfBuffer :: St -> Either String St
insert_focusedViewExpr_asChildOfBuffer st0 =
  prefixLeft "insert_focusedViewExpr_asChild:" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st0 ^? stGet_focusedBuffer
  pter :: PTree ExprRow <-
    maybe
    (Left "exprRowPorest or focusedSubtree not found.")
    Right $ b ^? ( bufferExprRowTree
                   . getFocusedSubtree . _Just )
  b' :: Buffer <- let
    f (VenExpr ve) = VenExpr $ ve { _viewExpr_showAsAddrs = mempty }
    f x = x -- should not happen
    in buffer_from_exprRowTree
       $ pter & pTreeLabel . viewExprNode %~ f
  redraw_focusedBuffer $
    hideReassurance . insertBuffer_asChild b' $ st0

insertBuffer_topNext :: Buffer -> St -> St
insertBuffer_topNext b =
  searchBuffers . _Just %~ insertLeaf_topNext b

insertBuffer_asChild :: Buffer -> St -> St
insertBuffer_asChild b =
  searchBuffers . _Just . P.focus . setFocusedSubtree
  %~ insertUnder_andFocus (pTreeLeaf b)

deleteFocused_buffer :: St -> St
deleteFocused_buffer st =
  case _searchBuffers st of
    Nothing -> st
    Just (p :: Porest Buffer) ->
      st & searchBuffers .~ deleteInPorest p

nudgeFocus_inBufferTree :: Direction -> St -> St
nudgeFocus_inBufferTree d =
  searchBuffers . _Just %~ nudgeFocus_inPorest d

nudgeFocused_buffer :: Direction -> St -> St
nudgeFocused_buffer d =
  searchBuffers . _Just %~ nudgeInPorest d
