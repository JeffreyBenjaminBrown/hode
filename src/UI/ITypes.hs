{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import Data.Map (Map)
import Lens.Micro
import Control.Lens.TH

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes
import Rslt.Show
import Util.Misc


-- | = Tiny types

-- | Some window names are used without ever reaching Brick. Ones that reach
-- Brick must be unique across windows in the same drawn image. (Not every
-- window Brick draws needs a name. Editors and viewports in particular do.)
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance deriving (Ord, Show, Eq)
data MainWindowName = CommandHistory
                    | Errors
                    | Results deriving (Ord, Show, Eq)

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder deriving (Show, Eq, Ord)

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

-- PITFALL: These types must come last in order to derive `Show`.

data Buffer = Buffer { _bufferQuery :: ViewQuery
                     , _bufferView  :: VTree RsltView
                     , _bufferPath  :: Path } deriving (Eq, Show, Ord)

makeLenses ''Buffer

data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far `focusRing` is unused in spirit, although technically used.
  , _buffers                :: Vorest Buffer
  , _vathToBuffer           :: Vath
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName
  , _commandHistory         :: [Command]
  , _appRslt                :: Rslt
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }

makeLenses ''St

-- TODO ? Dangerous: taking an `St` argument seems like it might
-- cause problems if lensing from an old `St` into a new one.
-- Specifically, if the two `St` have different `vathToBuffer`s.
stBuffer :: St -> Traversal' St Buffer
stBuffer st = buffers . atVath (st ^. vathToBuffer) . vTreeLabel

instance Show St where
  show st = "St { "
   ++ "buffer = "               ++ show (st ^. buffers)             ++ ",\n"
--   ++ "buffer = "               ++ show (st ^. buffers)             ++ ",\n"
   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
