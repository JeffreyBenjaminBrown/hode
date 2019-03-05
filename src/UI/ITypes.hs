{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

import           Data.Vector (Vector)
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
  , _results'  :: VQuery
  , _commands  :: B.Editor String Name
  , _appRslt   :: Rslt
  }

data VQuery = VQuery { -- "V" (for View) to distinguish it from Qseq.Query
    _vQueryString :: String
  , _vQueryResults :: Vector QueryResult }

data QueryResult = QueryResult {
    _resultExpr :: Expr
  , _subQueries :: Vector VQuery }

type Folder = String

data Command = CommandInsert Expr
             | CommandFind HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)

makeLenses ''St
makeLenses ''VQuery
makeLenses ''QueryResult
