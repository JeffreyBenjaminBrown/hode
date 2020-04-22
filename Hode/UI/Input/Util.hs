{-# LANGUAGE TemplateHaskell #-}

-- | PITFALL: Vty's `Meta` modifier, at least on my system,
-- cannot be used in conjunction with certain characters, such as ';'.

module Hode.UI.Input.Util (
    KeyCmd(..)
  , keyCmd_name, keyCmd_func, keyCmd_key, keyCmd_guide

  , paragraphs      -- ^ [String] -> String
  , paragraph       -- ^ [String] -> String
  , go  -- ^ (St -> St)               -> St -> B.EventM n (B.Next St)
  , goe -- ^ (St -> Either String St) -> St -> B.EventM n (B.Next St)
  , keyCmd_usePair  -- ^ KeyCmd -> (V.Event, St
                    -- -> B.EventM BrickName (B.Next St))
  , keyCmd_helpPair -- ^ KeyCmd -> (String, String)
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


data KeyCmd = KeyCmd
  { _keyCmd_name  :: String
  , _keyCmd_func  :: St -> B.EventM BrickName (B.Next St)
  , _keyCmd_key   :: (V.Key, [V.Modifier])
  , _keyCmd_guide :: String }
makeLenses ''KeyCmd

paragraphs :: [String] -> String
paragraphs = concat . L.intersperse "\n\n"

paragraph :: [String] -> String
paragraph = concat . L.intersperse " "

go :: (St -> St) -> St -> B.EventM n (B.Next St)
go f = B.continue . f . hideReassurance

goe :: (St -> Either String St) -> St -> B.EventM n (B.Next St)
goe f st = B.continue $ unEitherSt st $ f st

-- | `keyCmd_usePair kc` extracts the two fields of `kc` that the Hode UI
-- uses in its normal functioning to know what to execute in response to
-- what user input.
-- (The other two fields are used only in the interactive help.)
keyCmd_usePair :: KeyCmd -> (V.Event, St -> B.EventM BrickName (B.Next St))
keyCmd_usePair kc = ( uncurry V.EvKey $ _keyCmd_key kc
                    , _keyCmd_func kc )

-- | `keyCmd_helpPair kc` extracts the name and description of the `KeyCmd`,
-- for use in the interactive help.
-- (The other two fields are used only outside of the interactive help.)
keyCmd_helpPair :: KeyCmd -> (String, String)
keyCmd_helpPair kc = ( _keyCmd_name kc
                     , _keyCmd_guide kc )
