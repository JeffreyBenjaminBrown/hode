module Hode.UI.Types.Names where

import Hode.Hash.Types
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.Brick.Help.Types


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick.
-- Ones that do must be unique across windows in any drawn image.
-- Not every window Brick draws needs a name,
-- but editors and viewports in particular do.
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName     MainWindowName
               | BrickHelpName     HelpWindow
  deriving (Ord, Show, Eq)

data OptionalWindowName = LangCmds
                        | Reassurance
                        | Error
  deriving (Ord, Show, Eq)

data MainWindowName = LangCmdHistory
                    | SubgraphBuffer
                    | BufferBuffer
                    | HelpBuffer
  deriving (Ord, Show, Eq)

-- | PITFALL: This looks massively redundant given MainWindowName.
-- It would be, if they were both fields in the State type.
-- In fact, though, the State type only has a MainWindowName field.
-- Mode is computed as a function of that and other stuff.
data Mode = BufferMode
          | SubgraphMode
          | LangCmdMode
          | HelpMode
          | NoMode -- ^ For when there's nothing for the user to do but
                   -- switch to another context (which is always possible).
                   -- This might be reachable.
  deriving (Ord, Show, Eq)

data SubgraphSubmode =
    SubgraphSubmode_primary -- ^ most of the time
  | SubgraphSubmode_sort -- ^ to rearrange order
                         -- (without using the command window)
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
