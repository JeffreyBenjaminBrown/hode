module Hode.UI.ITypes.Names where

import Hode.Hash.HTypes
import Hode.Rslt.Binary
import Hode.Rslt.RTypes


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick. Ones that do reach
-- Brick must be unique across windows in any drawn image. (Not every
-- window Brick draws needs a name. Editors and viewports in particular do.)
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName
  deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance
  deriving (Ord, Show, Eq)
data MainWindowName = CommandHistory
                    | Results
                    | SearchBuffers
  deriving (Ord, Show, Eq)

data Command =
    CommandInsert       Expr
  | CommandReplace Addr Expr
  | CommandDelete  Addr
  | CommandFind     String HExpr
  | CommandFindSort String HExpr BinOrientation TpltAddr
  | CommandLoad Folder
  | CommandSave Folder
  deriving (Show, Eq, Ord)

type Folder = String
