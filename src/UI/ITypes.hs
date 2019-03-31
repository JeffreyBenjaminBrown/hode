{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes where

import           Control.Lens
import           Data.Map (Map)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hash.HTypes
import Rslt.RTypes
import Rslt.Show
import Util.Misc
import Util.PTree


-- | = Tiny types

-- | PITFALL: Some window names never reach Brick. Ones that do reach
-- Brick must be unique across windows in any drawn image. (Not every
-- window Brick draws needs a name. Editors and viewports in particular do.)
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance deriving (Ord, Show, Eq)
data MainWindowName = Buffers
                    | CommandHistory
                    | Results deriving (Ord, Show, Eq)

data Command = CommandInsert Expr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder deriving (Show, Eq, Ord)

type Folder = String


-- | = Views

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultString :: String } deriving (Show, Eq, Ord)

data ViewMembers = ViewMembers { _viewMembersCenter :: Addr }
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
-- `vcrCenter` appears. For instance, if the `Expr` at `Addr 3` helps some things,
-- then `ViewCenterRole 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data ViewCenterRole = ViewCenterRole {
    _vcrCenter :: Addr
  , _vcrRole   :: Role
  , _vcrTplt   :: Tplt Expr } deriving (Eq, Ord)

makePrisms ''RsltView -- prisms!
makeLenses ''ViewResult
makeLenses ''ViewMembers
makeLenses ''ViewCenterRole

instance Show ViewCenterRole where
  show vcr = let
    tplt = vcr ^. vcrTplt
    noLeft     = error "show ViewCenterRole: impossible"
    noRslt     = error "show ViewCenterRole: Rslt irrelevant"
    noMiscount = error "show ViewCenterRole: This math is good."
    showTplt = either (const noLeft) id
               $ eShow noRslt (ExprTplt tplt)
    in if vcr ^. vcrRole == RoleTplt
       then "Tplt " ++ showTplt
       else let (ar :: Arity) = length tplt - 1
                RoleMember (n :: Int) = vcr ^. vcrRole
                mbrs = either (const noMiscount) id
                       $ replaceNth (Phrase $ "it") n
                       $ replicate ar $ Phrase "_"
            in either (const noLeft) id
               $ eShow noRslt $ ExprRel $ Rel mbrs $ ExprTplt tplt

instance Show RsltView where
  show (VQuery x)      = "VQuery "      ++ show x
  show (VResult x)     = "VResult "     ++ show x
  show (VMembers x)    = "VMembers "    ++ show x
  show (VCenterRole x) = "VCenterRole " ++ show x


-- | = Huge types.

-- PITFALL: These types must come last in order to derive `Show`.

data Buffer = Buffer { _bufferQuery :: ViewQuery
                     , _bufferRsltViewTree  :: PTree RsltView
                     } deriving (Eq, Show, Ord)
makeLenses ''Buffer

data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far `focusRing` is unused in spirit, although technically used.
  , _buffers                :: Porest Buffer
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName
  , _commandHistory         :: [Command]
  , _appRslt                :: Rslt
  , _showingErrorWindow     :: Bool -- ^ overrides main window
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }
makeLenses ''St

stGetFocusedBuffer :: Getter St (Maybe Buffer)
stGetFocusedBuffer = to go where
  go :: St -> Maybe Buffer
  go st = st ^? buffers . P.focus . getFocusedSubtree . _Just . pTreeLabel

stSetFocusedBuffer :: Setter' St Buffer
stSetFocusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = buffers . P.focus . setFocusedSubtree .
         pTreeLabel %~ f

stGetFocusedRsltViewTree :: Getter St (Maybe (PTree RsltView))
stGetFocusedRsltViewTree = to go where
  go :: St -> Maybe (PTree RsltView)
  go st = st ^? stGetFocusedBuffer . _Just . bufferRsltViewTree . getFocusedSubtree . _Just

stSetFocusedRsltViewTree :: Setter' St (PTree RsltView)
stSetFocusedRsltViewTree = sets go where
  go :: (PTree RsltView -> PTree RsltView) -> St -> St
  go f = stSetFocusedBuffer . bufferRsltViewTree . setFocusedSubtree %~ f
