{-# LANGUAGE OverloadedStrings,
TemplateHaskell #-}

module Hode.Brick.Help.Types where

import Control.Lens
import qualified Data.List.PointedList as P
import           Data.Maybe


type Focused = Bool
type Choice3Plist = P.PointedList (String, String)
type Choice2Plist = P.PointedList (String, Choice3Plist)
type Choice1Plist = P.PointedList (String, Choice2Plist)

-- | Choice 1 determines what is available inn Choice 2,
-- and Choice 2 determines what is available in Choice 3.
-- Collectively, they determine what is in the content window.
data WindowName = Choice1 | Choice2 | Choice3 | Content
  deriving (Show, Eq, Ord)
makeLenses ''WindowName

allWindowNammes :: P.PointedList WindowName
allWindowNammes = fromJust $
                  P.fromList [ Choice1, Choice2, Choice3, Content ]

data Help = Help { _helpChoices :: Choice1Plist
                 , _helpWindows :: P.PointedList WindowName
                 , _helpHelp :: Bool }
  deriving (Show, Eq)
makeLenses ''Help

helpChoices2 :: Lens' Help Choice2Plist
helpChoices2 = helpChoices . P.focus . _2

helpChoices3 :: Lens' Help Choice3Plist
helpChoices3 = helpChoices . P.focus . _2 . P.focus . _2
