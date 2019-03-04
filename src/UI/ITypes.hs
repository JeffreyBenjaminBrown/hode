{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

import           Lens.Micro.TH

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes


data Name = Results | Commands
  deriving (Ord, Show, Eq)

data St = St {
    _focusRing :: B.FocusRing Name
  , _results   :: B.Editor String Name
  , _commands  :: B.Editor String Name
  , _appRslt   :: Rslt
  }

makeLenses ''St

type Folder = String

data Command = CommandInsert Expr
             | CommandFind HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)
