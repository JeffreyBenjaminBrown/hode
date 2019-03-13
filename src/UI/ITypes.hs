{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import           Data.Functor.Foldable.TH
import           Lens.Micro ((^.))
import           Lens.Micro.TH
import           Data.Vector (Vector)

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes


-- | = Tiny types

data WindowName = Results | Commands deriving (Ord, Show, Eq)

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)

data ShownInResultsWindow = ShowingError | ShowingResults
  deriving (Show,Eq, Ord)

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show,Eq, Ord)

type Folder = String


-- | = Meaty types

data St = St {
    _focusRing            :: B.FocusRing WindowName
  , _view                 :: ViewTree
  , _pathToFocus          :: [Int]
  , _uiError              :: String
  , _commands             :: B.Editor String WindowName
  , _appRslt              :: Rslt
  , _shownInResultsWindow :: ShownInResultsWindow
  }

data ViewTree = ViewTree {
    _viewFocus     :: Int -- ^ meaningless if `viewSubviews` empty
  , _viewIsFocused :: Bool
  , _viewContent   :: Either QueryView ResultView
  , _viewSubviews  :: Vector ViewTree -- ^ PITFALL: permits invalid state.
  -- A `ResultView`'s children should be `QueryView`s, and vice-versa.
  } deriving (Show)

type QueryView = String

data ResultView = ResultView {
    _viewResultAddr :: Addr
  , _viewResultExpr :: Expr
  , _viewResultString :: String } deriving (Show)

-- | `CenterRoleView` is used to group relationships in which the `Expr`at
-- `crvCenter` appears. For instance, if the `Expr` at `Addr 3` helps some things,
-- then `CenterRoleView 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data CenterRoleView = CenterRoleView {
  crvCenter :: Addr
  , crvRole :: Role
  , crvTplt :: [Expr] }

makeLenses ''St
makeLenses ''ViewTree
makeLenses ''ResultView

makeBaseFunctor ''ViewTree
makeLenses ''ViewTreeF

instance Show St where
  show st = "St { "
   ++ "view = "                 ++ show (st ^. view)                 ++ ",\n"
   ++ "pathToFocus = "          ++ show (st ^. pathToFocus)          ++ ",\n"
--   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
