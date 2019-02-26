{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.State where

import           Lens.Micro

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Rslt.RTypes
import UI.ITypes


-- | = functions involving St

initialState :: Rslt -> St
initialState r = St {
    _focusRing = F.focusRing [Results, Commands]
  , _results   = E.editor Results Nothing "" -- Maybe : line number limit
  , _commands  = E.editor Commands Nothing ""
  , _appRslt   = r
  , _history   = []
  }

editor_replaceText ::
  Lens' St (E.Editor String Name) -> (St -> St)
editor_replaceText windowGetter =
  windowGetter . E.editContentsL .~ Z.textZipper ["a","a","a"] Nothing
