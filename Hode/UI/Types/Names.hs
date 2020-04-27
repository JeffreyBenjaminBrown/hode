module Hode.UI.Types.Names where

import Hode.Hash.Types
import Hode.Rslt.Binary
import Hode.Rslt.Types


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick.
-- Ones that do must be unique across windows in any drawn image.
-- Not every window Brick draws needs a name,
-- but editors and viewports in particular do.
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName     MainWindowName
  deriving (Ord, Show, Eq)

data OptionalWindowName = LangCmds
                        | Reassurance
                        | Error
  deriving (Ord, Show, Eq)

data MainWindowName = LangCmdHistory
                    | SubgraphBuffer
                    | BufferBuffer
  deriving (Ord, Show, Eq)

data LangCmd =
    LangCmdInsert        Expr
  | LangCmdReplace  Addr Expr
  | LangCmdMove     Addr Addr
  | LangCmdDelete   Addr
  | LangCmdFind     String HExpr
  | LangCmdSort     String BinOrientation TpltAddr
  | LangCmdLoad     Folder
  | LangCmdSave     Folder
  deriving (Show, Eq, Ord)

type Folder = String
