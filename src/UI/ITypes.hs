{-# LANGUAGE TemplateHaskell #-}

module UI.ITypes where

import           Data.Vector (Vector)
import           Lens.Micro.TH

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes


-- | = Types used for names

data WindowName = Results | Commands
  deriving (Ord, Show, Eq)

-- | PITFALL: Permits invalid paths. A safer but more tedious path type
-- would use two edge types, and a path could only start from a query,
-- and a query could only lead to a result, and a result to a query,
-- and a path could never be empty.
data SubviewEdge = SvQuery String
                 | SvResult Addr deriving (Show, Eq, Ord)
type SubviewPath = [SubviewEdge]

data ShownInResultsWindow = ShowingError | ShowingResults

type Folder = String


-- | = meatier types

data St = St {
    _focusRing            :: B.FocusRing WindowName
  , _results              :: VQuery
  , _focusedResult        :: SubviewPath
  , _uiError              :: String
  , _commands             :: B.Editor String WindowName
  , _appRslt              :: Rslt
  , _shownInResultsWindow :: ShownInResultsWindow
  }

data VQuery = VQuery { -- "V" (for View) to distinguish it from Qseq.Query
    _vQueryPath :: SubviewPath -- ^ Path excluding the last (String) elt
  , _vQueryString :: String
  , _vQueryResults :: Vector QueryResult }

data QueryResult = QueryResult {
    _resultPath :: SubviewPath -- ^ Path excluding the last (Addr) elt
  , _resultAddr :: Addr
  , _resultExpr :: Expr
  , _resultString :: String
  , _subQueries :: Vector VQuery }

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)

makeLenses ''St
makeLenses ''VQuery
makeLenses ''QueryResult
