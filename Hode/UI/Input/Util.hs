-- | PITFALL: Vty's `Meta` modifier, at least on my system,
-- cannot be used in conjunction with certain characters, such as ';'.

module Hode.UI.Input.Util (
    paragraphs      -- ^ [String] -> String
  , paragraph       -- ^ [String] -> String
  , go  -- ^ (St -> St)               -> St -> B.EventM n (B.Next St)
  , goe -- ^ (St -> Either String St) -> St -> B.EventM n (B.Next St)
  ) where

import           Control.Lens hiding (folded)
import qualified Data.List             as L

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Graphics.Vty          as V

import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Util
import Hode.UI.Window


paragraphs :: [String] -> String
paragraphs = concat . L.intersperse "\n\n"

paragraph :: [String] -> String
paragraph = concat . L.intersperse " "

go :: (St -> St) -> St -> B.EventM n (B.Next St)
go f = B.continue . f . hideReassurance

goe :: (St -> Either String St) -> St -> B.EventM n (B.Next St)
goe f st = B.continue $ unEitherSt st $ f st
