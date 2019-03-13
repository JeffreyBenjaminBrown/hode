{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import           Data.Functor.Foldable.TH
import           Lens.Micro.TH
import           Data.Vector (Vector)

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes
--import UI.ITypes


-- | = Tiny types

data WindowName = Results | Commands deriving (Ord, Show, Eq)

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder
             deriving (Show, Eq, Ord)

-- | PITFALL: Permits invalid paths. A safer but more tedious path type
-- would use two edge types, and a path could only start from a query,
-- and a query could only lead to a result, and a result to a query,
-- and a path could never be empty.
data SubviewEdge = SvQuery String
                 | SvResult Addr deriving (Show, Eq, Ord)
type SubviewPath = [SubviewEdge]

data ShownInResultsWindow = ShowingError | ShowingResults

data Direction = DirUp | DirDown | DirLeft | DirRight

type Folder = String


-- | = Meaty types

data St2 = St2 {
    _st2_focusRing            :: B.FocusRing WindowName
  , _st2_view                 :: View
  , _st2_pathToFocus          :: [Int]
  , _st2_uiError              :: String
  , _st2_commands             :: B.Editor String WindowName
  , _st2_appRslt              :: Rslt
  , _st2_shownInResultsWindow :: ShownInResultsWindow
  }

data View = View {
    _viewFocus     :: Int -- ^ meaningless if `viewSubviews` empty
  , _viewIsFocused :: Bool
  , _viewContent   :: Either ViewQuery ViewResult
  , _viewSubviews  :: Vector View -- ^ PITFALL: permits invalid state.
  -- A `ViewResult`'s children should be `ViewQuery`s, and vice-versa.
  }

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultExpr :: Expr
  , _viewResultString :: String }

makeLenses ''St2
makeLenses ''View
makeLenses ''ViewResult

makeBaseFunctor ''View
makeLenses ''ViewF
