{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import Data.Map (Map)
import Data.Functor.Foldable.TH
import Data.Vector (Vector)
import Lens.Micro
import Control.Lens.TH


import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes
import Rslt.Show
import Util.Misc (replaceNth)


-- | = Tiny types

data WindowName = OptionalWindowName OptionalWindowName
                | MainWindowName MainWindowName deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance deriving (Ord, Show, Eq)
data MainWindowName = CommandHistory
                    | Errors
                    | Results deriving (Ord, Show, Eq)

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder deriving (Show, Eq, Ord)

type Path = [Int]

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show,Eq, Ord)

type Folder = String


-- | = Views

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultString :: String } deriving (Show, Eq, Ord)

data ViewMembers = ViewMembers { _mvCenter :: Addr }
  deriving (Show, Eq, Ord)

-- | PITFALL: In the case of `VTree RsltView`,
-- permits invalid state. Subviews of `VQuery`, `VMember`, `VCenterRole`
-- must be `VResult`s. The subviews of a `VResult` must be `VMember`s
-- or `VCenterRole`s. A `VQuery` can be nowhere but the top of the tree.
data RsltView = VQuery      ViewQuery
              | VResult     ViewResult
              | VMembers    ViewMembers
              | VCenterRole ViewCenterRole deriving (Eq, Ord)

-- | `ViewCenterRole` is used to group relationships in which the `Expr`at
-- `crvCenter` appears. For instance, if the `Expr` at `Addr 3` helps some things,
-- then `ViewCenterRole 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data ViewCenterRole = ViewCenterRole {
    _crvCenter :: Addr
  , _crvRole   :: Role
  , _crvTplt   :: [Expr] } deriving (Eq, Ord)

makePrisms ''RsltView -- prisms!
makeLenses ''ViewResult
makeLenses ''ViewMembers
makeLenses ''ViewCenterRole

instance Show ViewCenterRole where
  show vcr = let
    tplt = vcr ^. crvTplt
    noLeft     = error "show ViewCenterRole: impossible"
    noRslt     = error "show ViewCenterRole: Rslt irrelevant"
    noMiscount = error "show ViewCenterRole: This math is good."
    showTplt = either (const noLeft) id
               $ eShow noRslt (Tplt tplt)
    in if vcr ^. crvRole == RoleTplt
       then "Tplt " ++ showTplt
       else let (ar :: Arity) = length tplt - 1
                RoleMember (n :: Int) = vcr ^. crvRole
                mbrs = either (const noMiscount) id
                       $ replaceNth (Phrase $ "it") n
                       $ replicate ar $ Phrase "_"
            in either (const noLeft) id
               $ eShow noRslt $ Rel mbrs $ Tplt tplt

instance Show RsltView where
  show (VQuery x)      = "VQuery "      ++ show x
  show (VResult x)     = "VResult "     ++ show x
  show (VMembers x)    = "VMembers "    ++ show x
  show (VCenterRole x) = "VCenterRole " ++ show x


-- | = Huge types.

-- PITFALL: These types must come last to derive `Show`.

-- | A vector-based tree, for viewing things like Views, Buffers, ...
data VTree a = VTree {
  _vTreeLabel :: a
  , _vTrees :: Vector (VTree a)
  , _vTreeFocus :: Int -- ^ meaningless if `viewSubviews` empty
  , _vTreeIsFocused :: Bool
  } deriving (Eq, Show, Ord, Functor)

data St = St {
    _focusRing              :: B.FocusRing WindowName
    -- ^ So far `focusRing` is unused in spirit, although technically used.
  , _viewTree               :: VTree RsltView
  , _pathToFocus            :: Path
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String WindowName
  , _commandHistory         :: [Command]
  , _appRslt                :: Rslt
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }

makeLenses      ''St
makeLenses      ''VTree
makeBaseFunctor ''VTree
makeLenses      ''VTreeF

instance Show St where
  show st = "St { "
   ++ "view = "                 ++ show (st ^. viewTree)             ++ ",\n"
   ++ "pathToFocus = "          ++ show (st ^. pathToFocus)          ++ ",\n"
   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
