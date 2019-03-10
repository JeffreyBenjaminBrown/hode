{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes2 where

import           Data.Functor.Foldable.TH
import           Lens.Micro.TH
import           Data.Vector (Vector)

import Rslt.RTypes
import UI.ITypes


data View = View {
    _viewPath :: [SubviewEdge]
  , _viewFOcus :: Int
  , _viewContent :: Either ViewQuery ViewResult
  , _viewSubviews :: Vector View -- ^ PITFALL: permits invalid state.
  -- A `ViewResult`'s children should be `ViewQuery`s, and vice-versa.
  }

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultExpr :: Expr
  , _viewResultString :: String }

makeLenses ''View
makeLenses ''ViewResult
makeBaseFunctor ''View
