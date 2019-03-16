{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import           Data.Functor.Foldable.TH
import           Data.Vector (Vector)
import           Lens.Micro
import           Control.Lens.TH


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

type Path = [Int]

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show,Eq, Ord)

type Folder = String


-- | = Meaty types

data St = St {
    _focusRing            :: B.FocusRing WindowName
  , _viewTree             :: ViewTree
  , _pathToFocus          :: Path
  , _uiError              :: String
  , _commands             :: B.Editor String WindowName
  , _appRslt              :: Rslt
  , _shownInResultsWindow :: ShownInResultsWindow
  }

data ViewTree = ViewTree {
    _viewChildFocus     :: Int -- ^ meaningless if `viewSubviews` empty
  , _viewIsFocused :: Bool
  , _viewContent   :: View
  , _viewSubviews  :: Vector ViewTree -- ^ PITFALL: permits invalid state.
  -- The subviews of a `VQuery`, `VMember` or `VCenterRole`
  -- must be `VResult`s. The subviews of a `VResult` must be `VMember`s
  -- or `VCenterRole`s. A `VQuery` can be nowhere but the top of the tree.
  } deriving (Show)

data View = VQuery      ViewQuery
          | VResult     ViewResult
          | VMembers    ViewMembers
          | VCenterRole ViewCenterRole deriving (Show, Eq, Ord)

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultString :: String } deriving (Show, Eq, Ord)

data ViewMembers = ViewMembers { _mvCenter :: Addr }
  deriving (Show, Eq, Ord)

-- | `ViewCenterRole` is used to group relationships in which the `Expr`at
-- `crvCenter` appears. For instance, if the `Expr` at `Addr 3` helps some things,
-- then `ViewCenterRole 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data ViewCenterRole = ViewCenterRole {
    _crvCenter :: Addr
  , _crvRole :: Role
  , _crvTplt :: [Expr] } deriving (Show, Eq, Ord)

makeLenses ''St
makePrisms ''View -- prisms!
makeLenses ''ViewTree
makeLenses ''ViewResult
makeLenses ''ViewCenterRole

makeBaseFunctor ''ViewTree
makeLenses ''ViewTreeF


instance Show St where
  show st = "St { "
   ++ "view = "                 ++ show (st ^. viewTree)             ++ ",\n"
   ++ "pathToFocus = "          ++ show (st ^. pathToFocus)          ++ ",\n"
   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
