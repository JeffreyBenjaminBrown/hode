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
  , _view                 :: View
  , _pathToFocus          :: [Int]
  , _uiError              :: String
  , _commands             :: B.Editor String WindowName
  , _appRslt              :: Rslt
  , _shownInResultsWindow :: ShownInResultsWindow
  }

data View = View {
    _viewFocus     :: Int -- ^ meaningless if `viewSubviews` empty
  , _viewIsFocused :: Bool
  , _viewContent   :: Either ViewQuery ViewResult
  , _viewSubviews  :: Vector View -- ^ PITFALL: permits invalid state.
  -- A `ViewResult`'s children should be `ViewQuery`s, and vice-versa.
  } deriving (Show)

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultExpr :: Expr
  , _viewResultString :: String } deriving (Show)

makeLenses ''St
makeLenses ''View
makeLenses ''ViewResult

makeBaseFunctor ''View
makeLenses ''ViewF

instance Show St where
  show st = "St { "
   ++ "view = "                 ++ show (st ^. view)                 ++ ",\n"
   ++ "pathToFocus = "          ++ show (st ^. pathToFocus)          ++ ",\n"
--   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
