{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

import           Data.Map (Map)
import           Data.Vector (Vector)
import           Lens.Micro.TH

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes


data WindowName = Results | Commands
  deriving (Ord, Show, Eq)

-- | PITFALL: Permits invalid paths. A safer but more tedious path type
-- would use two edge types, and a path could only start from a query,
-- and a query could only lead to a result, and a result to a query,
-- and a path could never be empty.
data SubviewEdge = SvQuery String
                 | SvResult Addr deriving (Show, Eq, Ord)
type SubviewPath = [SubviewEdge]

data St = St {
    _focusRing    :: B.FocusRing WindowName
  , _results      :: VQuery
  , _uiError      :: String
  , _commands     :: B.Editor String WindowName
  , _appRslt      :: Rslt
  , _showingThing :: ShowingThing
  }

data ShowingThing = ShowingError | ShowingResults

data VQuery = VQuery { -- "V" (for View) to distinguish it from Qseq.Query
    _vQueryName :: SubviewPath
  , _vQueryString :: String
  , _vQueryResults :: Map Addr QueryResult }

data QueryResult = QueryResult {
    _resultName :: SubviewPath
  , _resultExpr :: Expr
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
