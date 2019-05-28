data Buffer = Buffer { _bufferQuery :: ViewQuery
                     , _bufferView  :: VTree ViewExprNode
                     , _bufferPath  :: Path } deriving (Eq, Ord)
makeLenses ''Buffer

data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far `focusRing` is unused in spirit, although technically used.
  , _buffers                :: Vorest Buffer
  , _puffers                :: Porest Puffer
  , _vathToBuffer           :: Vath
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName
  , _commandHistory         :: [Command]
  , _appRslt                :: Rslt
  , _showingErrorWindow     :: Bool -- ^ overrides main window
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }

-- TODO ? Dangerous: taking an `St` argument seems like it might
-- cause problems if lensing from an old `St` into a new one.
-- Specifically, if the two `St` have different `vathToBuffer`s.
stBuffer :: St -> Traversal' St Buffer
stBuffer st = buffers . atVath (st ^. vathToBuffer) . vTreeLabel
