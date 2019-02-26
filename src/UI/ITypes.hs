{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

import           Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Hash.HTypes
import Rslt.RTypes


data Name = Results | Commands
  deriving (Ord, Show, Eq)

data St = St {
    _focusRing :: F.FocusRing Name
  , _results   :: E.Editor String Name
  , _commands  :: E.Editor String Name
  , _appRslt   :: Rslt
  , _history   :: [HExpr]
  }

makeLenses ''St

type Folder = String

data Command = Insert Expr
             | Display HExpr
             | Load Folder
             | Save Folder
             deriving (Show, Eq, Ord)
