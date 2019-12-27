module Hode.UI.Types.Names where

import Hode.Hash.HTypes
import Hode.Rslt.Binary
import Hode.Rslt.RTypes


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick.
-- Ones that do must be unique across windows in any drawn image.
-- Not every window Brick draws needs a name,
-- but editors and viewports in particular do.
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName
  deriving (Ord, Show, Eq)

data OptionalWindowName = Commands
                        | Reassurance
  deriving (Ord, Show, Eq)

data MainWindowName = CommandHistory
                    | CycleBuffer
                    | SearchBuffer
                    | BufferBuffer
  deriving (Ord, Show, Eq)

data Command =
    CommandInsert       Expr
  | CommandReplace Addr Expr
  | CommandMove Addr Addr
  | CommandDelete  Addr
  | CommandFind     String HExpr
  | CommandFindSort String HExpr BinOrientation TpltAddr
  | CommandLoad Folder
  | CommandSave Folder
  deriving (Show, Eq, Ord)

type Folder = String
