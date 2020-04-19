{-# LANGUAGE OverloadedStrings,
TemplateHaskell #-}

module Hode.Brick.Help.Types where

import Control.Lens
import qualified Data.List.PointedList as P


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
allWindowNammes = maybe (error "impossible") id $
                  P.fromList [ Choice1, Choice2, Choice3, Content ]

data St = St { _choices :: Choice1Plist
             , _windows :: P.PointedList WindowName
             , _helpHelp :: Bool }
  deriving (Show, Eq)
makeLenses ''St

choices2 :: Lens' St Choice2Plist
choices2 = choices . P.focus . _2

choices3 :: Lens' St Choice3Plist
choices3 = choices . P.focus . _2 . P.focus . _2
