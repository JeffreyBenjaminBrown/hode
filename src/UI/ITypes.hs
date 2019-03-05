{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

--import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Vector (Vector)
import           Lens.Micro.TH

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes


data Name = Results | Commands
  deriving (Ord, Show, Eq)

data St = St {
    _focusRing    :: B.FocusRing Name
  , _results      :: B.Editor String Name
  , _results'     :: VQuery
  , _uiError      :: String
  , _commands     :: B.Editor String Name
  , _appRslt      :: Rslt
  , _showingThing :: ShowingThing
  }

data ShowingThing = ShowingError | ShowingResults

data VQuery = VQuery { -- "V" (for View) to distinguish it from Qseq.Query
    _vQueryString :: String
  , _vQueryResults :: Map Addr QueryResult }

data QueryResult = QueryResult {
    _resultExpr :: Expr
  , _resultString :: String
  , _subQueries :: Vector VQuery }

type Folder = String

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)

makeLenses ''St
makeLenses ''VQuery
makeLenses ''QueryResult
